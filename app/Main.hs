module Main where

import Control.Exception.Safe (Exception, MonadThrow, SomeException, bracket, throw, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, managed, runManaged)
import Data.Bits (Bits ((.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (packCString)
import Data.Foldable (Foldable (toList))
import Data.List (partition)
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Foreign.C (CInt)
import Foreign.C.String (CString)
import qualified SDL (Event (Event, eventPayload), EventPayload (QuitEvent), InitFlag (InitEvents, InitVideo), V2 (..), Window, WindowConfig (WindowConfig, windowGraphicsContext, windowInitialSize, windowResizable), WindowGraphicsContext (VulkanContext), createWindow, defaultWindow, destroyWindow, initialize, pollEvents, quit)
import qualified SDL as SDLT
import qualified SDL.Video.Vulkan as SDL (vkGetInstanceExtensions, vkLoadLibrary, vkUnloadLibrary)
import Vulkan (ApplicationInfo (apiVersion, applicationName, applicationVersion, engineName), DebugUtilsMessageSeverityFlagBitsEXT (DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT), DebugUtilsMessageTypeFlagBitsEXT (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT), DebugUtilsMessengerCreateInfoEXT (DebugUtilsMessengerCreateInfoEXT, messageSeverity, messageType), Device, ExtensionProperties (extensionName), Instance, InstanceCreateInfo (InstanceCreateInfo, applicationInfo, enabledExtensionNames, enabledLayerNames), Result (SUCCESS), SwapchainKHR, ValidationFeatureEnableEXT (VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT), ValidationFeaturesEXT, enabledValidationFeatures, enumerateInstanceExtensionProperties, enumerateInstanceLayerProperties, layerName, withDevice, withInstance, withSwapchainKHR, pattern API_VERSION_1_0, pattern EXT_DEBUG_UTILS_EXTENSION_NAME, pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME)
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import Vulkan.Extensions (ValidationFeaturesEXT (ValidationFeaturesEXT))
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

main :: IO ()
-- Create an instance and print its value
main = runManaged $ do
  initSDL

  --   inst <- withInstance zero Nothing allocate
  --   layers <- enumerateInstanceLayerProperties
  --   extentions <- enumerateInstanceExtensionProperties Nothing
  --   liftIO $ print inst *> print layers *> print extentions

  newSDLWindow appName SDL.defaultWindow $ \window -> pure ()

--   device <- newVulkanDevice
--   newVulkanWindow
--     device
--     appName
--     SDL.defaultWindow
--       { SDL.windowGraphicsContext = SDL.VulkanContext,
--         SDL.windowResizable = True,
--         SDL.windowInitialSize = windowDimention
--       }
--     $ \window swpachain ->
--       drawTriangle

drawTriangle :: MonadManaged m => m ()
drawTriangle = do
  pure ()

-- newVulkanDevice :: (MonadManaged m, MonadIO m) => m Device
-- newVulkanDevice = do
--   allocate_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
--   physicalDevice <- undefined
--   withDevice physicalDevice zero Nothing allocate

newVulkanWindow ::
  (MonadManaged m, MonadIO m) =>
  Device ->
  Text ->
  SDL.WindowConfig ->
  (SwapchainKHR -> SDL.Window -> m ()) ->
  m ()
newVulkanWindow device title config windowAction = do
  swapchain <- withSwapchainKHR device zero Nothing allocate
  newSDLWindow title config $
    windowAction swapchain

-- making vulkan instance
newVulkanInstance ::
  (MonadThrow m, MonadManaged m) =>
  SDL.Window ->
  m Instance
newVulkanInstance window = do
  instanceCreateInfo <- configureVulkanInstanceCreateInfo window
  withInstance instanceCreateInfo Nothing allocate

configureVulkanInstanceCreateInfo ::
  MonadIO m =>
  SDL.Window ->
  m
    ( InstanceCreateInfo
        '[ DebugUtilsMessengerCreateInfoEXT,
           ValidationFeaturesEXT
         ]
    )
configureVulkanInstanceCreateInfo window = do
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
    debugUtilsMessengerCreateInfoEXT :: DebugUtilsMessengerCreateInfoEXT
    debugUtilsMessengerCreateInfoEXT =
      zero
        { messageSeverity =
            DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
              .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
          messageType =
            DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
              .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
              .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
        }
    validationFeaturesEXT :: ValidationFeaturesEXT
    validationFeaturesEXT =
      zero
        { enabledValidationFeatures =
            Vector.fromList
              [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]
        }

configureVulkanExtensions ::
  MonadIO m => SDL.Window -> m (Vector ByteString)
configureVulkanExtensions window = do
  windowExtensions <-
    stringConvert
      =<< SDL.vkGetInstanceExtensions window
  availableExtensions <-
    toList . fmap extensionName
      <$> vulkanResultCheck
        (enumerateInstanceExtensionProperties Nothing)
  checkRequired
    (throwMIO . MissingRequiredExtension)
    availableExtensions
    $ requiredExtensions <> windowExtensions
  Vector.fromList
    <$> intersectOptional logger availableExtensions optionalExtensions

configureVulkanLayers ::
  MonadIO m => m (Vector ByteString)
configureVulkanLayers = do
  availableLayers <-
    toList . fmap layerName
      <$> vulkanResultCheck
        enumerateInstanceLayerProperties
  checkRequired
    (throwMIO . MissingRequiredLayer)
    availableLayers
    requiredLayers
  Vector.fromList
    <$> intersectOptional logger availableLayers optionalLayers

-- sdl
newSDLWindow ::
  (MonadManaged m, MonadIO m) =>
  Text ->
  SDL.WindowConfig ->
  (SDL.Window -> m ()) ->
  m ()
newSDLWindow title config windowAction = do
  window <-
    allocate
      (SDL.createWindow title config)
      SDL.destroyWindow
  sdlLoop $ windowAction window

sdlLoop :: MonadIO m => m () -> m ()
sdlLoop action = do
  willQuit <- any isQ <$> SDL.pollEvents
  unless willQuit $
    action *> sdlLoop action
  where
    isQ SDL.Event {SDL.eventPayload = SDL.QuitEvent} = True
    isQ _ = False

initSDL :: MonadManaged m => m ()
initSDL = do
  allocate_
    (SDL.initialize [SDL.InitVideo, SDL.InitEvents])
    SDL.quit

-- utility
instance Exception Result

data Error
  = MissingRequiredExtension ByteString
  | MissingRequiredLayer ByteString
  | OtherError
  deriving (Show, Eq)

instance Exception Error

vulkanResultCheck ::
  MonadIO m => m (Result, a) -> m a
vulkanResultCheck act = act >>= check
  where
    check (SUCCESS, x) = pure x
    check (err, _) = throwMIO err

intersectOptional ::
  MonadIO m => Eq a => (a -> m ()) -> [a] -> [a] -> m [a]
intersectOptional log available optional =
  let (present, missing) = partition (`elem` available) optional
   in present <$ mapM_ log missing

checkRequired ::
  (MonadIO m, Show a, Eq a) => (a -> m ()) -> [a] -> [a] -> m ()
checkRequired throw available required =
  let missing = filter (`elem` available) required
   in case missing of
        [] -> pure ()
        missing -> mapM_ throw missing

stringConvert :: MonadIO m => [CString] -> m [ByteString]
stringConvert = liftIO . traverse BS.packCString

logger :: (MonadIO m, Show a) => a -> m ()
logger = liftIO . print

throwMIO :: MonadIO m => Exception e => e -> m a
throwMIO = liftIO . throwIO

allocate ::
  MonadManaged m => IO a -> (a -> IO ()) -> m a
allocate alloc free = managed (bracket alloc free)

allocate_ :: MonadManaged m => IO a -> IO () -> m a
allocate_ alloc = allocate alloc . const
