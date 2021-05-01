module Main where

import Control.Exception.Safe (Exception, MonadCatch, SomeException (SomeException), bracket, throw, throwIO, try)
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
import qualified Data.Vector as Vector (fromList, maximumBy)
import Foreign (Word64, castPtr)
import Foreign.C (CInt)
import Foreign.C.String (CString)
import qualified SDL (Event (Event, eventPayload), EventPayload (QuitEvent), InitFlag (InitEvents, InitVideo), V2 (..), Window, WindowConfig (WindowConfig, windowGraphicsContext, windowInitialSize, windowResizable), WindowGraphicsContext (VulkanContext), createWindow, defaultWindow, destroyWindow, initialize, pollEvents, quit)
import qualified SDL as SDLT
import qualified SDL.Video.Vulkan as SDL (vkCreateSurface, vkGetInstanceExtensions, vkLoadLibrary, vkUnloadLibrary)
import UnliftIO.Resource (MonadResource, allocate, runResourceT)
import Vulkan (ApplicationInfo (apiVersion, applicationName, applicationVersion, engineName), DebugUtilsMessageSeverityFlagBitsEXT (DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT), DebugUtilsMessageTypeFlagBitsEXT (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT), DebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT, messageSeverity, messageType, pfnUserCallback), Device, DeviceCreateInfo (DeviceCreateInfo), ExtensionProperties (extensionName), ImageSwapchainCreateInfoKHR (swapchain), Instance (instanceHandle), InstanceCreateInfo (InstanceCreateInfo, applicationInfo, enabledExtensionNames, enabledLayerNames), MemoryHeap (size), PhysicalDevice (PhysicalDevice), PhysicalDeviceMemoryProperties (memoryHeaps), QueueFlagBits (QUEUE_GRAPHICS_BIT), Result (SUCCESS), ScreenSurfaceCreateInfoQNX (window), SurfaceKHR (SurfaceKHR), SwapchainKHR, ValidationFeatureEnableEXT (VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT), ValidationFeaturesEXT, destroySurfaceKHR, deviceName, enabledValidationFeatures, enumerateDeviceExtensionProperties, enumerateInstanceExtensionProperties, enumerateInstanceLayerProperties, enumeratePhysicalDevices, getPhysicalDeviceMemoryProperties, getPhysicalDeviceProperties, getPhysicalDeviceQueueFamilyProperties, layerName, message, submitDebugUtilsMessageEXT, withDebugUtilsMessengerEXT, withDevice, withInstance, withSwapchainKHR, pattern API_VERSION_1_0, pattern EXT_DEBUG_UTILS_EXTENSION_NAME, pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME, pattern KHR_SWAPCHAIN_EXTENSION_NAME)
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
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

requiredDeviceExtensions, optionalDeviceExtensions :: (Eq a, IsString a) => [a]
requiredDeviceExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME]
optionalDeviceExtensions = []

debugUtilsMessengerCreateInfoEXT ::
  DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfoEXT =
  zero
    { messageSeverity =
        DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
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
      DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
      DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
      zero {message = "Hello Vulkan from debug"}
    surface <- newSDLWindowVulkanSurface window vulkanInstance
    -- device <- newVulkanDevice vulkanInstance
    -- swapchain <- withSwapchainKHR device zero Nothing manageResource
    -- windowAction window swapchain
    pure ()

-- instantiate vulkan instance
newVulkanInstance ::
  (MonadResource m, MonadCatch m) =>
  SDL.Window ->
  m Instance
newVulkanInstance window = do
  instanceCreateInfo <- configureVulkanInstance window
  logger "instanceCreateInfo" instanceCreateInfo
  ins <- withInstance instanceCreateInfo Nothing manageResource
  liftIO $ putStrLn "new Vulkan Instance"
  pure ins

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

newVulkanDevice ::
  (MonadResource m, MonadCatch m) =>
  Instance ->
  m Device
newVulkanDevice vulkanInstance = do
  physicalDevices <-
    vulkanResultCheck $
      enumeratePhysicalDevices vulkanInstance
  (physicalDevice, deviceExtensions) <-
    pickVulkanPhysicalDevice physicalDevices
  logger "selected PhysicalDevice name" . deviceName
    =<< getPhysicalDeviceProperties physicalDevice
  logger "DeviceExtensions" deviceExtensions
  deviceCreateInfo <- configureVulkanDevice physicalDevice
  withDevice physicalDevice deviceCreateInfo Nothing manageResource

pickVulkanPhysicalDevice ::
  forall m.
  (MonadResource m, MonadCatch m) =>
  Vector PhysicalDevice ->
  m (PhysicalDevice, Vector ByteString)
pickVulkanPhysicalDevice physicalDevices = do
  physicalDevicesHasRequiedExtensions <-
    rights . toList
      <$> mapM
        ( \physicalDevice ->
            (try @m @SomeException) $
              do
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
                totalMemory <-
                  sum . fmap size . memoryHeaps
                    <$> getPhysicalDeviceMemoryProperties
                      physicalDevice
                pure
                  ( totalMemory,
                    ( physicalDevice,
                      availableDeviceExtensions
                    )
                  )
        )
        physicalDevices
  when (null physicalDevicesHasRequiedExtensions) $
    throw $
      NoPhysicalDeviceWithRequiredExtensions requiredExtensions
  let (selectedPhysicalDevice, availableDeviceExtensions) =
        snd $
          maximumBy
            (comparing fst)
            physicalDevicesHasRequiedExtensions
  (,) selectedPhysicalDevice
    . Vector.fromList
    . (requiredDeviceExtensions <>)
    <$> intersectOptional
      "Device Extensions"
      availableDeviceExtensions
      optionalDeviceExtensions

configureVulkanDevice ::
  (MonadResource m, MonadCatch m) =>
  PhysicalDevice ->
  m (DeviceCreateInfo '[])
configureVulkanDevice physicalDevice = do
  queueFamilyProperties <-
    getPhysicalDeviceQueueFamilyProperties physicalDevice
  -- unless
  --   ( isFlagged
  --       QUEUE_GRAPHICS_BIT
  --       $ queueFlags queueFamilyProperties
  --   ) $ throw
  error "not implemented"

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
