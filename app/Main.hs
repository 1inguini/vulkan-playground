module Main where

import Control.Exception.Safe (Exception, MonadCatch, MonadThrow, SomeException (SomeException), bracket, throw, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Bits (Bits (zeroBits, (.&.), (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (packCString)
import Data.Either (rights)
import Data.Foldable (Foldable (toList), maximumBy)
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (elemIndices, filter, filterM, findIndex, findIndices, foldM, fromList, generate, length, maximumBy, singleton)
import Foreign (Word32, Word64, castPtr)
import Foreign.C (CInt)
import Foreign.C.String (CString)
import qualified SDL (Event (Event, eventPayload), EventPayload (QuitEvent), InitFlag (InitEvents, InitVideo), V2 (..), Window, WindowConfig (WindowConfig, windowGraphicsContext, windowInitialSize, windowResizable), WindowGraphicsContext (VulkanContext), createWindow, defaultWindow, destroyWindow, initialize, pollEvents, quit)
import qualified SDL as SDLT
import qualified SDL.Video.Vulkan as SDL (vkCreateSurface, vkGetInstanceExtensions, vkLoadLibrary, vkUnloadLibrary)
import UnliftIO.Resource (MonadResource, allocate, runResourceT)
import Vulkan (ApplicationInfo (apiVersion, applicationName, applicationVersion, engineName), ColorSpaceKHR (COLOR_SPACE_SRGB_NONLINEAR_KHR), DebugUtilsMessageSeverityFlagBitsEXT (DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT), DebugUtilsMessageTypeFlagBitsEXT (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT), DebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT, messageSeverity, messageType, pfnUserCallback), Device (deviceHandle), DeviceCreateInfo (DeviceCreateInfo, enabledExtensionNames, queueCreateInfos), DeviceQueueCreateInfo (DeviceQueueCreateInfo, queueFamilyIndex, queuePriorities), ExtensionProperties (extensionName), Format (FORMAT_B8G8R8A8_UNORM), ImageSwapchainCreateInfoKHR (swapchain), Instance (instanceHandle), InstanceCreateInfo (InstanceCreateInfo, applicationInfo, enabledExtensionNames, enabledLayerNames), MemoryHeap (size), PhysicalDevice (PhysicalDevice), PhysicalDeviceMemoryProperties (memoryHeaps), QueueFamilyProperties (queueCount), QueueFlagBits (QUEUE_GRAPHICS_BIT), Result (SUCCESS), ScreenSurfaceCreateInfoQNX (window), SharingMode (SHARING_MODE_CONCURRENT, SHARING_MODE_EXCLUSIVE), SurfaceCapabilities2KHR (surfaceCapabilities), SurfaceCapabilitiesKHR (minImageCount), SurfaceFormatKHR (colorSpace, format), SurfaceKHR (SurfaceKHR), SwapchainCreateInfoKHR (SwapchainCreateInfoKHR, minImageCount, surface), SwapchainKHR, ValidationFeatureEnableEXT (VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT), ValidationFeaturesEXT, destroySurfaceKHR, deviceHandle, deviceName, enabledValidationFeatures, enumerateDeviceExtensionProperties, enumerateInstanceExtensionProperties, enumerateInstanceLayerProperties, enumeratePhysicalDevices, getPhysicalDeviceMemoryProperties, getPhysicalDeviceProperties, getPhysicalDeviceQueueFamilyProperties, getPhysicalDeviceSurfaceCapabilitiesKHR, getPhysicalDeviceSurfaceFormatsKHR, getPhysicalDeviceSurfaceSupportKHR, layerName, message, queueFlags, submitDebugUtilsMessageEXT, withDebugUtilsMessengerEXT, withDevice, withInstance, withSwapchainKHR, pattern API_VERSION_1_0, pattern EXT_DEBUG_UTILS_EXTENSION_NAME, pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME, pattern KHR_SWAPCHAIN_EXTENSION_NAME)
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct), pattern (:&), pattern (::&))
import Vulkan.Extensions (ValidationFeaturesEXT (ValidationFeaturesEXT))
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Zero (zero)

-- info
appName :: IsString a => a
appName = "Haskell Vulkan triangle example"

appInfo :: ApplicationInfo
appInfo =
  zero
    { applicationName = Just appName,
      apiVersion = API_VERSION_1_0
    }

windowDimention :: SDL.V2 CInt -- width, height
windowDimention = SDL.V2 800 600

requiredExtensions, optionalExtensions :: (IsString a, Eq a) => [a]
requiredExtensions = [EXT_DEBUG_UTILS_EXTENSION_NAME]
optionalExtensions = [EXT_VALIDATION_FEATURES_EXTENSION_NAME]

requiredLayers, optionalLayers :: (IsString a, Eq a) => [a]
requiredLayers = []
optionalLayers = ["VK_LAYER_KHRONOS_validation"]

desiredSurfaceFormat :: SurfaceFormatKHR
desiredSurfaceFormat =
  zero
    { format = FORMAT_B8G8R8A8_UNORM,
      colorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR
    }

requiredDeviceExtensions, optionalDeviceExtensions :: (Eq a, IsString a) => [a]
requiredDeviceExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME]
optionalDeviceExtensions = []

debugUtilsMessengerCreateInfoEXT ::
  DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfoEXT =
  zero
    { messageSeverity =
        DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      messageType =
        DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      pfnUserCallback = debugCallbackPtr
    }

main :: IO ()
-- Create an instance and print its value
main = runResourceT $ do
  initSDL

  --   inst <- withInstance zero Nothing manageResource
  --   layers <- enumerateInstanceLayerProperties
  --   extentions <- enumerateInstanceExtensionProperties Nothing
  --   liftIO $ print inst *> print layers *> print extentions

  newVulkanWindow
    appName
    SDL.defaultWindow
      { SDL.windowGraphicsContext = SDL.VulkanContext,
        SDL.windowResizable = False,
        SDL.windowInitialSize = windowDimention
      }
    $ \window swapchain ->
      pure ()

drawTriangle :: MonadResource m => m ()
drawTriangle = do
  pure ()

newVulkanWindow ::
  (MonadResource m, MonadCatch m, MonadIO m) =>
  Text ->
  SDL.WindowConfig ->
  (SDL.Window -> SwapchainKHR -> m ()) ->
  m ()
newVulkanWindow title config windowAction =
  newSDLWindow title config $ \window -> do
    vulkanInstance <- newVulkanInstance window
    withDebugUtilsMessengerEXT
      vulkanInstance
      debugUtilsMessengerCreateInfoEXT
      Nothing
      manageResource
    submitDebugUtilsMessageEXT
      vulkanInstance
      DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
      DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
      zero {message = "Hello Vulkan from debug"}
    surface <- newSDLWindowVulkanSurface window vulkanInstance
    ( physicalDevice,
      deviceCreateInfo,
      swapchainCreateInfo
      ) <-
      pickVulkanPhysicalDevice vulkanInstance surface
    -- device <-
    --   newVulkanDevice
    --     vulkanInstance
    --     physicalDevice
    --     deviceExtensions
    --     graphicsQueueFamilyIndex
    --     presentQueueFamilyIndex
    device <- withDevice physicalDevice deviceCreateInfo Nothing manageResource
    logger "created Vulkan Device" $ deviceHandle device
    -- swapchain <-
    --   newVulkanSwapchain
    --     surface
    --     physicalDevice
    --     graphicsQueueFamilyIndex
    --     presentQueueFamilyIndex
    swapchain <- withSwapchainKHR device swapchainCreateInfo Nothing manageResource
    logger "created Vulkan" swapchain
    windowAction window swapchain

-- instantiate vulkan instance
newVulkanInstance ::
  (MonadResource m, MonadCatch m) =>
  SDL.Window ->
  m Instance
newVulkanInstance window = do
  instanceCreateInfo <- configureVulkanInstance window
  logger "instanceCreateInfo" instanceCreateInfo
  vulkanInstance <- withInstance instanceCreateInfo Nothing manageResource
  logger "created Vulkan Instance" $ instanceHandle vulkanInstance
  pure vulkanInstance

configureVulkanInstance ::
  (MonadIO m, MonadCatch m) =>
  SDL.Window ->
  m
    ( InstanceCreateInfo
        '[ DebugUtilsMessengerCreateInfoEXT,
           ValidationFeaturesEXT
         ]
    )
configureVulkanInstance window = do
  extensions <- configureVulkanExtensions window
  layers <- configureVulkanLayers
  pure $
    zero
      { applicationInfo = Just appInfo,
        enabledLayerNames = layers,
        enabledExtensionNames = extensions
      }
      ::& debugUtilsMessengerCreateInfoEXT
        :& validationFeaturesEXT
        :& ()
  where
    validationFeaturesEXT :: ValidationFeaturesEXT
    validationFeaturesEXT =
      zero
        { enabledValidationFeatures =
            Vector.fromList
              [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]
        }

configureVulkanExtensions ::
  (MonadCatch m, MonadIO m) =>
  SDL.Window ->
  m (Vector ByteString)
configureVulkanExtensions window = do
  windowExtensions <-
    stringConvert
      =<< SDL.vkGetInstanceExtensions window
  logger "extensions required from sdl" windowExtensions
  availableExtensions <-
    toList . fmap extensionName
      <$> vulkanResultCheck
        (enumerateInstanceExtensionProperties Nothing)
  let requiredExtensions' =
        requiredExtensions
          <> windowExtensions
  checkRequired
    MissingRequiredExtension
    availableExtensions
    requiredExtensions'
  logger "extensions passed check" requiredExtensions'
  Vector.fromList . (requiredExtensions' <>)
    <$> intersectOptional
      "Extensions"
      availableExtensions
      optionalExtensions

configureVulkanLayers ::
  (MonadCatch m, MonadIO m) => m (Vector ByteString)
configureVulkanLayers = do
  availableLayers <-
    toList . fmap layerName
      <$> vulkanResultCheck
        enumerateInstanceLayerProperties
  logger "availableLayers" availableLayers
  checkRequired
    MissingRequiredLayer
    availableLayers
    requiredLayers
  logger "layers passed check" requiredLayers
  Vector.fromList . (requiredLayers <>)
    <$> intersectOptional "Layers" availableLayers optionalLayers

-- newVulkanDevice ::
--   (MonadResource m, MonadCatch m) =>
--   Instance ->
--   PhysicalDevice ->
--   Vector ByteString ->
--   Word32 ->
--   Word32 ->
--   m Device
-- newVulkanDevice vulkanInstance physicalDevice deviceExtensions graphicsQueueFamilyIndex presentQueueFamilyIndex = do
--   logger "selected PhysicalDevice name" . deviceName
--     =<< getPhysicalDeviceProperties physicalDevice
--   logger "graphicsQueueFamilyIndex" graphicsQueueFamilyIndex
--   logger "presentQueueFamilyIndex" presentQueueFamilyIndex
--   let deviceCreateInfo =
--         zero
--           { queueCreateInfos = do
--               index <-
--                 if graphicsQueueFamilyIndex == presentQueueFamilyIndex
--                   then Vector.singleton graphicsQueueFamilyIndex
--                   else
--                     Vector.fromList
--                       [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
--               pure $
--                 SomeStruct $
--                   zero
--                     { queueFamilyIndex = index,
--                       queuePriorities = Vector.singleton 1
--                     },
--             enabledExtensionNames = deviceExtensions
--           }
--   logger "DeviceCreateInfo" deviceCreateInfo
--   device <-
--     withDevice physicalDevice deviceCreateInfo Nothing manageResource
--   logger "created" $ deviceHandle device
--   pure device

pickVulkanPhysicalDevice ::
  forall m.
  (MonadResource m, MonadCatch m) =>
  Instance ->
  SurfaceKHR ->
  -- m (PhysicalDevice, Vector ByteString, Word32, Word32)
  m (PhysicalDevice, DeviceCreateInfo '[], SwapchainCreateInfoKHR '[])
pickVulkanPhysicalDevice vulkanInstance surface = do
  physicalDevices <-
    vulkanResultCheck $
      enumeratePhysicalDevices vulkanInstance
  physicalDevicesPassed <-
    rights . toList
      <$> mapM
        ( \physicalDevice -> (try @m @Error) $ do
            logger "examining" . deviceName
              =<< getPhysicalDeviceProperties physicalDevice
            logger "SurfaceFormats"
              =<< getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
            ( graphicsQueueFamilyIndex,
              presentQueueFamilyIndex
              ) <-
              checkQueueFamily physicalDevice surface
            availableDeviceExtensions <-
              checkDeviceExtensions physicalDevice
            totalMemory <-
              sum . fmap size . memoryHeaps
                <$> getPhysicalDeviceMemoryProperties
                  physicalDevice
            pure
              ( totalMemory,
                ( physicalDevice,
                  availableDeviceExtensions,
                  graphicsQueueFamilyIndex,
                  presentQueueFamilyIndex
                )
              )
        )
        physicalDevices
  when (null physicalDevicesPassed) $
    throw $
      NoPhysicalDeviceWithRequiredExtensions requiredExtensions
  let ( selectedPhysicalDevice,
        availableDeviceExtensions,
        graphicsQueueFamilyIndex,
        presentQueueFamilyIndex
        ) = snd $ maximumBy (comparing fst) physicalDevicesPassed
  selectedDeviceExtensions <-
    Vector.fromList
      . (requiredDeviceExtensions <>)
      <$> intersectOptional
        "Device Extensions"
        availableDeviceExtensions
        optionalDeviceExtensions
  let deviceCreateInfo =
        zero
          { queueCreateInfos = do
              index <-
                if graphicsQueueFamilyIndex == presentQueueFamilyIndex
                  then Vector.singleton graphicsQueueFamilyIndex
                  else
                    Vector.fromList
                      [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
              pure $
                SomeStruct $
                  zero
                    { queueFamilyIndex = index,
                      queuePriorities = Vector.singleton 1
                    },
            enabledExtensionNames = selectedDeviceExtensions
          }
  pure
    ( selectedPhysicalDevice,
      deviceCreateInfo,
      undefined
    )

checkDeviceExtensions ::
  (MonadCatch m, MonadIO m) =>
  PhysicalDevice ->
  m [ByteString]
checkDeviceExtensions physicalDevice = do
  availableDeviceExtensions <-
    toList . fmap extensionName
      <$> vulkanResultCheck
        ( enumerateDeviceExtensionProperties
            physicalDevice
            Nothing
        )
  checkRequired
    MissingRequiredPhysicalDeviceExtensions
    availableDeviceExtensions
    requiredDeviceExtensions
  pure availableDeviceExtensions

checkQueueFamily ::
  (MonadCatch m, MonadIO m) =>
  PhysicalDevice ->
  SurfaceKHR ->
  m (Word32, Word32)
checkQueueFamily physicalDevice surface = do
  queueFamilyProperties <-
    getPhysicalDeviceQueueFamilyProperties
      physicalDevice
  graphicsQueueFamilyIndex <-
    fromIntegral @Int @Word32
      <$> headThrow
        NoGraphicsQueue
        ( Vector.findIndex
            ( \vulkanQueue ->
                isFlagged
                  QUEUE_GRAPHICS_BIT
                  (queueFlags vulkanQueue)
                  && (0 < queueCount vulkanQueue)
            )
            queueFamilyProperties
        )
  presentQueueFamilyIndex <-
    headThrow NoPresentQueue
      =<< Vector.foldM
        ( \mayPassedIndex queueIndex ->
            case mayPassedIndex of
              Nothing -> do
                hasPresent <-
                  getPhysicalDeviceSurfaceSupportKHR
                    physicalDevice
                    queueIndex
                    surface
                pure $
                  if hasPresent
                    then Just queueIndex
                    else Nothing
              justIndex -> pure justIndex
        )
        Nothing
        ( Vector.generate
            (Vector.length queueFamilyProperties)
            fromIntegral
        )
  pure (graphicsQueueFamilyIndex, presentQueueFamilyIndex)

-- checkSurfaceFormat ::
--   (MonadCatch m, MonadIO m) =>
--     PhysicalDevice -> SurfaceKHR -> m

-- newVulkanSwapchain ::
--   (MonadResource m, MonadCatch m) =>
--   SurfaceKHR ->
--   PhysicalDevice ->
--   Word32 ->
--   Word32 ->
--   m SwapchainKHR
-- newVulkanSwapchain surface physicalDevice graphicsQueueFamilyIndex presentQueueFamilyIndex = do
--   undefined

configureVulkanSwapchain ::
  (MonadResource m, MonadCatch m) =>
  SurfaceKHR ->
  PhysicalDevice ->
  Word32 ->
  Word32 ->
  m (SwapchainCreateInfoKHR '[])
configureVulkanSwapchain surface physicalDevice graphicsQueueFamilyIndex presentQueueFamilyIndex = do
  surfaceCapabilities <-
    getPhysicalDeviceSurfaceCapabilitiesKHR
      physicalDevice
      surface
  pure $
    zero
      { surface = surface,
        minImageCount = minImageCount (surfaceCapabilities :: SurfaceCapabilitiesKHR) + 1
      }
  where
    (sharingMode, queueFamilyIndices) =
      if graphicsQueueFamilyIndex == presentQueueFamilyIndex
        then (SHARING_MODE_EXCLUSIVE, mempty)
        else
          ( SHARING_MODE_CONCURRENT,
            Vector.fromList [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
          )

-- sdl
initSDL :: MonadResource m => m ()
initSDL = do
  manageResource_
    (SDL.initialize [SDL.InitVideo, SDL.InitEvents])
    SDL.quit

newSDLWindow ::
  (MonadResource m, MonadIO m) =>
  Text ->
  SDL.WindowConfig ->
  (SDL.Window -> m ()) ->
  m ()
newSDLWindow title config windowAction = do
  window <-
    manageResource
      (SDL.createWindow title config)
      SDL.destroyWindow
  -- TODO: 最後にloopを有効化
  -- sdlLoop $ windowAction window
  windowAction window

sdlLoop :: MonadIO m => m () -> m ()
sdlLoop action = do
  willQuit <- any isQ <$> SDL.pollEvents
  unless willQuit $
    action *> sdlLoop action
  where
    isQ SDL.Event {SDL.eventPayload = SDL.QuitEvent} = True
    isQ _ = False

newSDLWindowVulkanSurface ::
  MonadResource m =>
  SDL.Window ->
  Instance ->
  m SurfaceKHR
newSDLWindowVulkanSurface window vulkanInstance =
  manageResource
    ( SurfaceKHR
        <$> SDL.vkCreateSurface
          window
          (castPtr $ instanceHandle vulkanInstance)
    )
    $ \surface ->
      destroySurfaceKHR vulkanInstance surface Nothing

-- utility
instance Exception Result

data Error
  = MissingRequiredExtension ByteString
  | MissingRequiredLayer ByteString
  | MissingRequiredDeviceExtension ByteString
  | MissingRequiredPhysicalDeviceExtensions ByteString
  | NoPhysicalDeviceWithRequiredExtensions [ByteString]
  | NoGraphicsQueue
  | NoPresentQueue
  | OtherError
  deriving (Show, Eq)

instance Exception Error

vulkanResultCheck ::
  MonadCatch m => m (Result, a) -> m a
vulkanResultCheck act = act >>= check
  where
    check (SUCCESS, x) = pure x
    check (err, _) = throw err

intersectOptional ::
  MonadIO m => (Show a, Eq a) => String -> [a] -> [a] -> m [a]
intersectOptional typeName available optional =
  let (present, missing) = partition (`elem` available) optional
   in present <$ case missing of
        [] -> pure ()
        _ -> logger ("Missing optional " <> typeName) missing

headThrow ::
  (Exception e, Foldable f, MonadThrow m) => e -> f a -> m a
headThrow err xs =
  case toList xs of
    [] -> throw err
    (x : _) -> pure x

checkRequired ::
  (MonadCatch m, Eq a, Exception e) =>
  (a -> e) ->
  [a] ->
  [a] ->
  m ()
checkRequired wrap available required =
  let missing = getMissing available required
   in case missing of
        [] -> pure ()
        missing -> mapM_ (throw . wrap) missing

getMissing :: Eq a => [a] -> [a] -> [a]
getMissing available =
  filter (`notElem` available)

isFlagged :: Bits a => a -> a -> Bool
isFlagged flagBit flag = zeroBits /= flagBit .&. flag

stringConvert :: MonadIO m => [CString] -> m [ByteString]
stringConvert = liftIO . traverse BS.packCString

logger :: (MonadIO m, Show a) => String -> a -> m ()
logger message =
  liftIO
    . (*>)
      (putStr (message <> " "))
    . print

manageResource ::
  MonadResource m => IO a -> (a -> IO ()) -> m a
-- manageResource alloc free = managed (bracket alloc free)
manageResource alloc free = snd <$> allocate alloc free

manageResource_ :: MonadResource m => IO a -> IO () -> m a
manageResource_ alloc = manageResource alloc . const
