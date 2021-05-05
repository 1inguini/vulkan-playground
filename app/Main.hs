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
import qualified Data.Vector as Vector (elemIndices, filter, filterM, findIndex, findIndices, foldM, fromList, generate, length, maximumBy, partition, singleton)
import Foreign (Word32, Word64, castPtr)
import Foreign.C (CInt)
import Foreign.C.String (CString)
import qualified SDL (Event (Event, eventPayload), EventPayload (QuitEvent), InitFlag (InitEvents, InitVideo), V2 (..), Window, WindowConfig (WindowConfig, windowGraphicsContext, windowInitialSize, windowResizable), WindowGraphicsContext (VulkanContext), createWindow, defaultWindow, destroyWindow, initialize, pollEvents, quit)
import qualified SDL as SDLT
import qualified SDL.Video.Vulkan as SDL (vkCreateSurface, vkGetInstanceExtensions, vkLoadLibrary, vkUnloadLibrary)
import UnliftIO.Resource (MonadResource, allocate, runResourceT)
import Vulkan (ApplicationInfo (apiVersion, applicationName, applicationVersion, engineName), ColorSpaceKHR (COLOR_SPACE_SRGB_NONLINEAR_KHR), CompositeAlphaFlagBitsKHR (COMPOSITE_ALPHA_OPAQUE_BIT_KHR), DebugUtilsMessageSeverityFlagBitsEXT (DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT), DebugUtilsMessageTypeFlagBitsEXT (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT), DebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT, messageSeverity, messageType, pfnUserCallback), Device (deviceHandle), DeviceCreateInfo (DeviceCreateInfo, enabledExtensionNames, queueCreateInfos), DeviceQueueCreateInfo (DeviceQueueCreateInfo, queueFamilyIndex, queuePriorities), ExtensionProperties (extensionName), Extent2D (Extent2D), Format (FORMAT_B8G8R8A8_UNORM), Image, ImageSwapchainCreateInfoKHR (swapchain), ImageUsageFlagBits (IMAGE_USAGE_COLOR_ATTACHMENT_BIT), ImageView, ImageViewCreateInfo (..), ImageViewType (IMAGE_VIEW_TYPE_2D), Instance (instanceHandle), InstanceCreateInfo (InstanceCreateInfo, applicationInfo, enabledExtensionNames, enabledLayerNames), MemoryHeap (size), PhysicalDevice (PhysicalDevice), PhysicalDeviceMemoryProperties (memoryHeaps), PresentModeKHR (PRESENT_MODE_FIFO_KHR, PRESENT_MODE_IMMEDIATE_KHR, PRESENT_MODE_MAILBOX_KHR), QueueFamilyProperties (queueCount), QueueFlagBits (QUEUE_GRAPHICS_BIT), Result (INCOMPLETE, SUCCESS), ScreenSurfaceCreateInfoQNX (window), SharingMode (SHARING_MODE_CONCURRENT, SHARING_MODE_EXCLUSIVE), SurfaceCapabilities2KHR (surfaceCapabilities), SurfaceCapabilitiesKHR (SurfaceCapabilitiesKHR, currentExtent, currentTransform, minImageCount), SurfaceFormatKHR (SurfaceFormatKHR, colorSpace, format), SurfaceKHR (SurfaceKHR), SwapchainCreateInfoKHR (SwapchainCreateInfoKHR, clipped, compositeAlpha, imageArrayLayers, imageColorSpace, imageExtent, imageFormat, imageSharingMode, imageUsage, minImageCount, preTransform, presentMode, queueFamilyIndices, surface), SwapchainKHR, TraceRaysIndirectCommandKHR (height), ValidationFeatureEnableEXT (VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT), ValidationFeaturesEXT, destroySurfaceKHR, deviceHandle, deviceName, enabledValidationFeatures, enumerateDeviceExtensionProperties, enumerateInstanceExtensionProperties, enumerateInstanceLayerProperties, enumeratePhysicalDevices, getPhysicalDeviceMemoryProperties, getPhysicalDeviceProperties, getPhysicalDeviceQueueFamilyProperties, getPhysicalDeviceSurfaceCapabilitiesKHR, getPhysicalDeviceSurfaceFormatsKHR, getPhysicalDeviceSurfacePresentModesKHR, getPhysicalDeviceSurfaceSupportKHR, getSwapchainImagesKHR, imageColorSpace, layerName, message, queueFlags, submitDebugUtilsMessageEXT, withDebugUtilsMessengerEXT, withDevice, withImageView, withInstance, withSwapchainKHR, pattern API_VERSION_1_0, pattern EXT_DEBUG_UTILS_EXTENSION_NAME, pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME, pattern KHR_SWAPCHAIN_EXTENSION_NAME)
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

windowWidth, windowHeight :: Integral a => a
windowWidth = 800
windowHeight = 600

requiredExtensions, optionalExtensions :: (IsString a, Eq a) => Vector a
requiredExtensions = [EXT_DEBUG_UTILS_EXTENSION_NAME]
optionalExtensions = [EXT_VALIDATION_FEATURES_EXTENSION_NAME]

requiredLayers, optionalLayers :: (IsString a, Eq a) => Vector a
requiredLayers = []
optionalLayers = ["VK_LAYER_KHRONOS_validation"]

desiredSurfaceFormats :: Vector SurfaceFormatKHR
desiredSurfaceFormats =
  [ zero
      { format = FORMAT_B8G8R8A8_UNORM,
        colorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR
      }
  ]

desiredPresentMode :: Vector PresentModeKHR
desiredPresentMode =
  [ PRESENT_MODE_MAILBOX_KHR,
    PRESENT_MODE_FIFO_KHR,
    PRESENT_MODE_IMMEDIATE_KHR
  ]

requiredDeviceExtensions, optionalDeviceExtensions :: (Eq a, IsString a) => Vector a
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
        SDL.windowInitialSize = SDL.V2 windowWidth windowHeight
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
      swapchainCreateInfo@SwapchainCreateInfoKHR {imageFormat = imageFormat}
      ) <-
      pickVulkanPhysicalDevice vulkanInstance surface
    device <- withDevice physicalDevice deviceCreateInfo Nothing manageResource
    logger "created Vulkan Device" $ deviceHandle device
    swapchain <- withSwapchainKHR device swapchainCreateInfo Nothing manageResource
    logger "created Vulkan" swapchain
    imageViews <-
      mapM (newImageView device imageFormat)
        =<< vulkanResultCheck (getSwapchainImagesKHR device swapchain)
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
  logger "created Vulkan" $ instanceHandle vulkanInstance
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
    traverse stringConvert . Vector.fromList
      =<< SDL.vkGetInstanceExtensions window
  logger "extensions required from sdl" windowExtensions
  availableExtensions <-
    fmap extensionName
      <$> vulkanResultCheck
        (enumerateInstanceExtensionProperties Nothing)
  logger "availableExtensions" availableExtensions
  let requiredExtensions' = requiredExtensions <> windowExtensions
  checkRequired
    MissingRequiredExtension
    availableExtensions
    requiredExtensions'
  logger "extensions passed check" requiredExtensions'
  (requiredExtensions' <>)
    <$> intersectOptional
      "Extensions"
      availableExtensions
      optionalExtensions

configureVulkanLayers ::
  (MonadCatch m, MonadIO m) => m (Vector ByteString)
configureVulkanLayers = do
  availableLayers <-
    fmap layerName
      <$> vulkanResultCheck
        enumerateInstanceLayerProperties
  logger "availableLayers" availableLayers
  checkRequired
    MissingRequiredLayer
    availableLayers
    requiredLayers
  logger "layers passed check" requiredLayers
  (requiredLayers <>)
    <$> intersectOptional "Layers" availableLayers optionalLayers

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
            surfaceFormats <-
              vulkanResultCheck $
                getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
            logger "SurfaceFormats" surfaceFormats
            selectedSurfaceFormat <-
              headThrow (MissingDesiredSurfaceFormat desiredSurfaceFormats)
                =<< intersectOptional "SurfaceFormat" surfaceFormats desiredSurfaceFormats
            logger "SurfaceFormat passed check" selectedSurfaceFormat
            presentModes <-
              vulkanResultCheck $
                getPhysicalDeviceSurfacePresentModesKHR
                  physicalDevice
                  surface
            logger "PresentModes" presentModes
            selectedPresentMode <-
              headThrow (MissingDesiredPresentMode desiredPresentMode)
                =<< intersectOptional "PresentMode" presentModes desiredPresentMode
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
                  presentQueueFamilyIndex,
                  selectedSurfaceFormat,
                  selectedPresentMode
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
        presentQueueFamilyIndex,
        selectedSurfaceFormat,
        selectedPresentMode
        ) = snd $ maximumBy (comparing fst) physicalDevicesPassed
  selectedDeviceExtensions <-
    (requiredDeviceExtensions <>)
      <$> intersectOptional
        "Device Extensions"
        availableDeviceExtensions
        optionalDeviceExtensions
  let deviceCreateInfo =
        configureVulkanDevice
          selectedDeviceExtensions
          graphicsQueueFamilyIndex
          presentQueueFamilyIndex
  swapchainCreateInfo <-
    configureVulkanSwapchain
      surface
      selectedPhysicalDevice
      graphicsQueueFamilyIndex
      presentQueueFamilyIndex
      selectedSurfaceFormat
      selectedPresentMode
  pure
    ( selectedPhysicalDevice,
      deviceCreateInfo,
      swapchainCreateInfo
    )

checkDeviceExtensions ::
  (MonadCatch m, MonadIO m) =>
  PhysicalDevice ->
  m (Vector ByteString)
checkDeviceExtensions physicalDevice = do
  availableDeviceExtensions <-
    fmap extensionName
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
    fromIntegral
      <$> headThrow
        NoGraphicsQueue
        ( Vector.findIndex
            ( \vulkanQueue ->
                (0 < queueCount vulkanQueue)
                  && isFlagged QUEUE_GRAPHICS_BIT (queueFlags vulkanQueue)
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

configureVulkanDevice ::
  Vector ByteString -> Word32 -> Word32 -> DeviceCreateInfo '[]
configureVulkanDevice deviceExtensions graphicsQueueFamilyIndex presentQueueFamilyIndex =
  zero
    { queueCreateInfos = do
        index <-
          if graphicsQueueFamilyIndex == presentQueueFamilyIndex
            then [graphicsQueueFamilyIndex]
            else [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
        pure $
          SomeStruct $
            zero
              { queueFamilyIndex = index,
                queuePriorities = [1]
              },
      enabledExtensionNames = deviceExtensions
    }

configureVulkanSwapchain ::
  (MonadResource m, MonadCatch m) =>
  SurfaceKHR ->
  PhysicalDevice ->
  Word32 ->
  Word32 ->
  SurfaceFormatKHR ->
  PresentModeKHR ->
  m (SwapchainCreateInfoKHR '[])
configureVulkanSwapchain
  surface
  physicalDevice
  graphicsQueueFamilyIndex
  presentQueueFamilyIndex
  SurfaceFormatKHR {format = format, colorSpace = colorSpace}
  presentMode =
    do
      SurfaceCapabilitiesKHR
        { minImageCount = minImageCount,
          currentExtent = currentExtent,
          currentTransform = currentTransform
        } <-
        getPhysicalDeviceSurfaceCapabilitiesKHR
          physicalDevice
          surface
      pure $
        zero
          { surface = surface,
            minImageCount = minImageCount + 1,
            imageFormat = format,
            imageColorSpace = colorSpace,
            imageExtent = case currentExtent of
              Extent2D width height
                | width == maxBound && height == maxBound ->
                  Extent2D windowWidth windowHeight
              _ -> currentExtent,
            imageArrayLayers = 1,
            imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
            imageSharingMode = sharingMode,
            queueFamilyIndices = queueFamilyIndices,
            preTransform = currentTransform,
            compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
            presentMode = presentMode,
            clipped = True
          }
    where
      (sharingMode, queueFamilyIndices)
        | graphicsQueueFamilyIndex == presentQueueFamilyIndex =
          (SHARING_MODE_EXCLUSIVE, [])
        | otherwise =
          ( SHARING_MODE_CONCURRENT,
            [graphicsQueueFamilyIndex, presentQueueFamilyIndex] :: Vector Word32
          )

newImageView ::
  MonadResource m =>
  Device ->
  SurfaceFormatKHR ->
  Image ->
  m ImageView
newImageView device surfaceFormat image =
  withImageView device (configureImageView surfaceFormat image) Nothing manageResource

configureImageView :: SurfaceFormatKHR -> Image -> ImageViewCreateInfo '[]
configureImageView surfaceFormat image =
  zero
    { image = image,
      viewType = IMAGE_VIEW_TYPE_2D,
      format = undefined
    }

-- sdl
initSDL :: MonadResource m => m ()
initSDL = do
  manageResource_
    (SDL.initialize @[] [SDL.InitVideo, SDL.InitEvents])
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
  | MissingRequiredSurfaceFormat SurfaceFormatKHR
  | NoPhysicalDeviceWithRequiredExtensions (Vector ByteString)
  | MissingDesiredSurfaceFormat (Vector SurfaceFormatKHR)
  | MissingDesiredPresentMode (Vector PresentModeKHR)
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
    check (INCOMPLETE, x) = pure x
    check (err, _) = throw err

intersectOptional ::
  MonadIO m => (Show a, Eq a) => String -> Vector a -> Vector a -> m (Vector a)
intersectOptional typeName available optional =
  let (present, missing) = Vector.partition (`elem` available) optional
   in present
        <$ if null missing
          then pure ()
          else logger ("Missing optional " <> typeName) missing

headThrow ::
  (Exception e, Foldable f, MonadThrow m) => e -> f a -> m a
headThrow err xs =
  case toList xs of
    [] -> throw err
    (x : _) -> pure x

checkRequired ::
  (MonadCatch m, Eq a, Exception e) =>
  (a -> e) ->
  Vector a ->
  Vector a ->
  m ()
checkRequired wrap available required =
  let missing = getMissing available required
   in if null missing
        then pure ()
        else mapM_ (throw . wrap) missing

getMissing :: Eq a => Vector a -> Vector a -> Vector a
getMissing available =
  Vector.filter (`notElem` available)

isFlagged :: Bits a => a -> a -> Bool
isFlagged flagBit flag = zeroBits /= flagBit .&. flag

stringConvert :: MonadIO m => CString -> m ByteString
stringConvert = liftIO . BS.packCString

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
