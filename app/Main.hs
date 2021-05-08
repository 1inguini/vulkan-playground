module Main where

import Control.Exception.Safe
  ( Exception,
    MonadCatch,
    MonadThrow,
    SomeException (SomeException),
    bracket,
    throw,
    throwIO,
    try,
  )
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Bits (Bits (zeroBits, (.&.), (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (packCString)
import Data.Either (rights)
import Data.Foldable (Foldable (toList), maximumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
  ( filter,
    findIndex,
    fromList,
    generate,
    length,
    partition,
    zip,
  )
import Foreign (Word32, Word64, castPtr)
import Foreign.C (CInt)
import Foreign.C.String (CString)
import qualified SDL
  ( Event (Event, eventPayload),
    EventPayload (QuitEvent),
    InitFlag (InitEvents, InitVideo),
    V2 (V2),
    Window,
    WindowConfig
      ( windowGraphicsContext,
        windowInitialSize,
        windowResizable
      ),
    WindowGraphicsContext (VulkanContext),
    createWindow,
    defaultWindow,
    destroyWindow,
    initialize,
    pollEvents,
    quit,
  )
import qualified SDL.Video.Vulkan as SDL
  ( vkCreateSurface,
    vkGetInstanceExtensions,
    vkLoadLibrary,
    vkUnloadLibrary,
  )
import UnliftIO.Resource
  ( MonadResource,
    allocate,
    runResourceT,
  )
import Vulkan
  ( AccessFlagBits (ACCESS_COLOR_ATTACHMENT_READ_BIT, ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
    ApplicationInfo
      ( apiVersion,
        applicationName,
        applicationVersion,
        engineName
      ),
    AttachmentDescription
      ( finalLayout,
        format,
        initialLayout,
        loadOp,
        samples,
        stencilLoadOp,
        stencilStoreOp,
        storeOp
      ),
    AttachmentLoadOp (ATTACHMENT_LOAD_OP_CLEAR, ATTACHMENT_LOAD_OP_DONT_CARE),
    AttachmentReference (attachment, layout),
    AttachmentStoreOp (ATTACHMENT_STORE_OP_DONT_CARE, ATTACHMENT_STORE_OP_STORE),
    BindSparseInfo (signalSemaphores),
    ClearColorValue (Float32),
    ClearValue (Color),
    ColorComponentFlagBits (COLOR_COMPONENT_A_BIT, COLOR_COMPONENT_B_BIT, COLOR_COMPONENT_G_BIT, COLOR_COMPONENT_R_BIT),
    ColorSpaceKHR (COLOR_SPACE_SRGB_NONLINEAR_KHR),
    CommandBuffer (commandBufferHandle),
    CommandBufferAllocateInfo (commandBufferCount, commandPool, level),
    CommandBufferBeginInfo (flags),
    CommandBufferInheritanceRenderPassTransformInfoQCOM (renderArea),
    CommandBufferLevel (COMMAND_BUFFER_LEVEL_PRIMARY),
    CommandBufferUsageFlagBits (COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT),
    CommandPool,
    CommandPoolCreateInfo (queueFamilyIndex),
    ComponentMapping (a, b, g, r),
    ComponentSwizzle (COMPONENT_SWIZZLE_IDENTITY),
    CompositeAlphaFlagBitsKHR (COMPOSITE_ALPHA_OPAQUE_BIT_KHR),
    CullModeFlagBits (CULL_MODE_NONE),
    DebugUtilsMessageSeverityFlagBitsEXT
      ( DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
        DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
      ),
    DebugUtilsMessageTypeFlagBitsEXT
      ( DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT,
        DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
      ),
    DebugUtilsMessengerCallbackDataEXT (message),
    DebugUtilsMessengerCreateInfoEXT
      ( messageSeverity,
        messageType,
        pfnUserCallback
      ),
    Device (deviceHandle),
    DeviceCreateInfo
      ( enabledExtensionNames,
        queueCreateInfos
      ),
    DeviceQueueCreateInfo (queueFamilyIndex, queuePriorities),
    ExtensionProperties (extensionName),
    Extent2D (Extent2D, height, width),
    Format (FORMAT_B8G8R8A8_UNORM),
    Framebuffer,
    FramebufferAttachmentImageInfo (layerCount),
    FramebufferCreateInfo (attachments, height, layers, renderPass, width),
    FrontFace (FRONT_FACE_CLOCKWISE),
    GeometryAABBNV (offset),
    GraphicsPipelineCreateInfo
      ( basePipelineHandle,
        colorBlendState,
        depthStencilState,
        dynamicState,
        inputAssemblyState,
        layout,
        multisampleState,
        rasterizationState,
        renderPass,
        stages,
        subpass,
        vertexInputState,
        viewportState
      ),
    Image,
    ImageAspectFlagBits (IMAGE_ASPECT_COLOR_BIT),
    ImageLayout (IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_PRESENT_SRC_KHR, IMAGE_LAYOUT_UNDEFINED),
    ImageSubresourceRange (aspectMask, baseArrayLayer, baseMipLevel, layerCount, levelCount),
    ImageSwapchainCreateInfoKHR (swapchain),
    ImageUsageFlagBits (IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
    ImageView,
    ImageViewCreateInfo (components, format, image, subresourceRange, viewType),
    ImageViewType (IMAGE_VIEW_TYPE_2D),
    InputAttachmentAspectReference (subpass),
    Instance (instanceHandle),
    InstanceCreateInfo
      ( applicationInfo,
        enabledExtensionNames,
        enabledLayerNames
      ),
    LayerProperties (layerName),
    MemoryBarrier2KHR (dstStageMask, srcStageMask),
    MemoryHeap (size),
    Offset2D (Offset2D, x, y),
    PhysicalDevice,
    PhysicalDeviceMemoryProperties (memoryHeaps),
    PhysicalDeviceProperties (deviceName),
    Pipeline,
    PipelineBindPoint (PIPELINE_BIND_POINT_GRAPHICS),
    PipelineColorBlendAttachmentState (blendEnable, colorWriteMask),
    PipelineColorBlendStateCreateInfo (attachments, logicOpEnable),
    PipelineInputAssemblyStateCreateInfo (primitiveRestartEnable, topology),
    PipelineMultisampleStateCreateInfo
      ( minSampleShading,
        rasterizationSamples,
        sampleMask,
        sampleShadingEnable
      ),
    PipelineRasterizationStateCreateInfo
      ( cullMode,
        depthBiasEnable,
        depthClampEnable,
        frontFace,
        lineWidth,
        polygonMode,
        rasterizerDiscardEnable
      ),
    PipelineShaderStageCreateInfo (module', name, stage),
    PipelineStageFlagBits (PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
    PipelineViewportStateCreateInfo (scissors, viewports),
    PolygonMode (POLYGON_MODE_FILL),
    PresentInfoKHR (imageIndices, swapchains, waitSemaphores),
    PresentModeKHR
      ( PRESENT_MODE_FIFO_KHR,
        PRESENT_MODE_IMMEDIATE_KHR,
        PRESENT_MODE_MAILBOX_KHR
      ),
    PrimitiveTopology (PRIMITIVE_TOPOLOGY_TRIANGLE_LIST),
    QueueFamilyProperties
      ( queueCount,
        queueFlags
      ),
    QueueFlagBits (QUEUE_GRAPHICS_BIT),
    Rect2D (Rect2D, extent, offset),
    RenderPass,
    RenderPassBeginInfo (clearValues, framebuffer, renderArea, renderPass),
    RenderPassCreateInfo (attachments, dependencies, subpasses),
    Result (INCOMPLETE, SUCCESS),
    SampleCountFlagBits (SAMPLE_COUNT_1_BIT),
    ShaderModuleCreateInfo (code),
    ShaderStageFlagBits (SHADER_STAGE_FRAGMENT_BIT, SHADER_STAGE_VERTEX_BIT),
    SharingMode (SHARING_MODE_CONCURRENT, SHARING_MODE_EXCLUSIVE),
    SubmitInfo (commandBuffers, signalSemaphores, waitDstStageMask, waitSemaphores),
    SubpassContents (SUBPASS_CONTENTS_INLINE),
    SubpassDependency
      ( dstAccessMask,
        dstStageMask,
        dstSubpass,
        srcAccessMask,
        srcStageMask,
        srcSubpass
      ),
    SubpassDescription (colorAttachments, pipelineBindPoint),
    SurfaceCapabilitiesKHR
      ( SurfaceCapabilitiesKHR,
        currentExtent,
        currentTransform,
        minImageCount
      ),
    SurfaceFormatKHR (SurfaceFormatKHR, colorSpace, format),
    SurfaceKHR (SurfaceKHR),
    SwapchainCreateInfoKHR
      ( SwapchainCreateInfoKHR,
        clipped,
        compositeAlpha,
        imageArrayLayers,
        imageColorSpace,
        imageExtent,
        imageFormat,
        imageSharingMode,
        imageUsage,
        minImageCount,
        preTransform,
        presentMode,
        queueFamilyIndices,
        surface
      ),
    SwapchainKHR,
    ValidationFeatureEnableEXT
      ( VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT
      ),
    ValidationFeaturesEXT (ValidationFeaturesEXT),
    Viewport (Viewport, height, maxDepth, minDepth, width, x, y),
    acquireNextImageKHR,
    cmdBindPipeline,
    cmdDraw,
    cmdUseRenderPass,
    destroySurfaceKHR,
    enabledValidationFeatures,
    enumerateDeviceExtensionProperties,
    enumerateInstanceExtensionProperties,
    enumerateInstanceLayerProperties,
    enumeratePhysicalDevices,
    getDeviceQueue,
    getPhysicalDeviceMemoryProperties,
    getPhysicalDeviceProperties,
    getPhysicalDeviceQueueFamilyProperties,
    getPhysicalDeviceSurfaceCapabilitiesKHR,
    getPhysicalDeviceSurfaceFormatsKHR,
    getPhysicalDeviceSurfacePresentModesKHR,
    getPhysicalDeviceSurfaceSupportKHR,
    getSwapchainImagesKHR,
    queuePresentKHR,
    queueSubmit,
    submitDebugUtilsMessageEXT,
    useCommandBuffer,
    withCommandBuffers,
    withCommandPool,
    withDebugUtilsMessengerEXT,
    withDevice,
    withFramebuffer,
    withGraphicsPipelines,
    withImageView,
    withInstance,
    withPipelineLayout,
    withRenderPass,
    withSemaphore,
    withShaderModule,
    withSwapchainKHR,
    pattern API_VERSION_1_0,
    pattern EXT_DEBUG_UTILS_EXTENSION_NAME,
    pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME,
    pattern KHR_SWAPCHAIN_EXTENSION_NAME,
    pattern SUBPASS_EXTERNAL,
  )
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct), pattern (:&), pattern (::&))
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Utils.ShaderQQ.Backend.Glslang.Internal (compileShaderQ)
import Vulkan.Utils.ShaderQQ.ShaderType (ShaderType (HLSL))
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

vertShader, fragShader :: (ByteString, ByteString)
vertShader =
  ( "main",
    $(liftIO (readFile "app/vert.hlsl") >>= compileShaderQ Nothing HLSL "vert" (Just "main"))
  )
fragShader =
  ( "main",
    $(liftIO (readFile "app/frag.hlsl") >>= compileShaderQ Nothing HLSL "frag" (Just "main"))
  )

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
    -- $ \window swapchain ->
    --   pure ()

drawTriangle :: MonadResource m => m ()
drawTriangle = do
  pure ()

newVulkanWindow ::
  (MonadResource m, MonadCatch m, MonadIO m) =>
  Text ->
  SDL.WindowConfig ->
  -- (SDL.Window -> SwapchainKHR -> m ()) ->
  m ()
newVulkanWindow title config = do
  window <- newSDLWindow title config
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
    swapchainCreateInfo@SwapchainCreateInfoKHR
      { imageFormat = imageFormat,
        imageExtent = imageExtent
      },
    graphicsQueueFamilyIndex,
    presentQueueFamilyIndex
    ) <-
    pickVulkanPhysicalDevice vulkanInstance surface
  device <- withDevice physicalDevice deviceCreateInfo Nothing manageResource
  graphicsQueue <- getDeviceQueue device graphicsQueueFamilyIndex 0
  presentQueue <- getDeviceQueue device presentQueueFamilyIndex 0
  logger "created Vulkan Device" $ deviceHandle device
  swapchain <- logCreation $ withSwapchainKHR device swapchainCreateInfo Nothing manageResource
  imageViews <-
    mapM
      (newImageView device imageFormat)
      =<< vulkanResultCheck (getSwapchainImagesKHR device swapchain)
  renderPass <- newRenderPass device imageFormat
  pipeline <- newVulkanPipeline device renderPass imageExtent
  framebuffers <-
    mapM
      (newVulkanFramebuffer device renderPass imageExtent)
      imageViews
  commandPool <- newVulkanCommandPool device graphicsQueueFamilyIndex
  commandBufferHandles <-
    fmap commandBufferHandle
      <$> newVulkanCommandBuffers
        device
        renderPass
        pipeline
        framebuffers
        commandPool
        imageExtent
  imageAvailableSemaphore <- withSemaphore device zero Nothing manageResource
  renderFinishedSemaphore <- withSemaphore device zero Nothing manageResource
  -- sdlLoop $ do
  imageIndex <-
    vulkanResultCheck $
      acquireNextImageKHR device swapchain maxBound imageAvailableSemaphore zero
  queueSubmit
    graphicsQueue
    [ SomeStruct
        zero
          { waitSemaphores = [imageAvailableSemaphore],
            waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
            commandBuffers = [commandBufferHandles ! fromIntegral imageIndex],
            signalSemaphores = [renderFinishedSemaphore]
          }
    ]
    zero
  vulkanResultCheck $
    flip (,) ()
      <$> queuePresentKHR
        presentQueue
        zero
          { waitSemaphores = [renderFinishedSemaphore],
            swapchains = [swapchain],
            imageIndices = [imageIndex]
          }
  sdlLoop $ pure ()

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
  m (InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT, ValidationFeaturesEXT])
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
      zero {enabledValidationFeatures = [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]}

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
  checkRequired MissingRequiredExtensions availableExtensions requiredExtensions'
  logger "extensions passed check" requiredExtensions'
  (requiredExtensions' <>)
    <$> intersectOptional "Extensions" availableExtensions optionalExtensions

configureVulkanLayers ::
  (MonadCatch m, MonadIO m) => m (Vector ByteString)
configureVulkanLayers = do
  availableLayers <- fmap layerName <$> vulkanResultCheck enumerateInstanceLayerProperties
  logger "availableLayers" availableLayers
  checkRequired MissingRequiredLayers availableLayers requiredLayers
  logger "layers passed check" requiredLayers
  (requiredLayers <>)
    <$> intersectOptional "Layers" availableLayers optionalLayers

pickVulkanPhysicalDevice ::
  forall m.
  (MonadResource m, MonadCatch m) =>
  Instance ->
  SurfaceKHR ->
  -- m (PhysicalDevice, Vector ByteString, Word32, Word32)
  m (PhysicalDevice, DeviceCreateInfo '[], SwapchainCreateInfoKHR '[], Word32, Word32)
pickVulkanPhysicalDevice vulkanInstance surface = do
  physicalDevices <- vulkanResultCheck $ enumeratePhysicalDevices vulkanInstance
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
            (graphicsQueueFamilyIndex, presentQueueFamilyIndex) <-
              checkQueueFamily physicalDevice surface
            availableDeviceExtensions <- checkDeviceExtensions physicalDevice
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
    throw $ NoPhysicalDeviceWithRequiredExtensions requiredExtensions
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
      swapchainCreateInfo,
      graphicsQueueFamilyIndex,
      presentQueueFamilyIndex
    )

checkDeviceExtensions ::
  (MonadCatch m, MonadIO m) =>
  PhysicalDevice ->
  m (Vector ByteString)
checkDeviceExtensions physicalDevice = do
  availableDeviceExtensions <-
    fmap extensionName
      <$> vulkanResultCheck (enumerateDeviceExtensionProperties physicalDevice Nothing)
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
  queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties physicalDevice
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
      =<< foldM
        ( \mayPassedIndex queueIndex ->
            case mayPassedIndex of
              Nothing -> do
                hasPresent <- getPhysicalDeviceSurfaceSupportKHR physicalDevice queueIndex surface
                pure $ if hasPresent then Just queueIndex else Nothing
              justIndex -> pure justIndex
        )
        Nothing
        (Vector.generate (Vector.length queueFamilyProperties) fromIntegral)
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
        pure $ SomeStruct $ zero {queueFamilyIndex = index, queuePriorities = [1]},
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
        getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
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
  Format ->
  Image ->
  m ImageView
newImageView device format image =
  logCreation $ withImageView device (configureVulkanImageView format image) Nothing manageResource

configureVulkanImageView ::
  Format -> Image -> ImageViewCreateInfo '[]
configureVulkanImageView format image =
  zero
    { image = image,
      viewType = IMAGE_VIEW_TYPE_2D,
      format = format,
      components =
        zero
          { r = COMPONENT_SWIZZLE_IDENTITY,
            g = COMPONENT_SWIZZLE_IDENTITY,
            b = COMPONENT_SWIZZLE_IDENTITY,
            a = COMPONENT_SWIZZLE_IDENTITY
          },
      subresourceRange =
        zero
          { aspectMask = IMAGE_ASPECT_COLOR_BIT,
            baseMipLevel = 0,
            levelCount = 1,
            baseArrayLayer = 0,
            layerCount = 1
          }
    }

newRenderPass ::
  MonadResource m => Device -> Format -> m RenderPass
newRenderPass device format =
  let renderPassCreateInfo = configureVulkanRenderPass format
   in logCreation $ withRenderPass device renderPassCreateInfo Nothing manageResource

configureVulkanRenderPass :: Format -> RenderPassCreateInfo '[]
configureVulkanRenderPass format =
  zero
    { attachments =
        [ zero
            { format = format,
              samples = SAMPLE_COUNT_1_BIT,
              loadOp = ATTACHMENT_LOAD_OP_CLEAR,
              storeOp = ATTACHMENT_STORE_OP_STORE,
              stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE,
              stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE,
              initialLayout = IMAGE_LAYOUT_UNDEFINED,
              finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
            }
        ],
      subpasses =
        [ zero
            { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS,
              colorAttachments =
                [zero {attachment = 0, layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL}]
            }
        ],
      dependencies =
        [ zero
            { srcSubpass = SUBPASS_EXTERNAL,
              dstSubpass = 0,
              srcStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
              srcAccessMask = zero,
              dstStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
              dstAccessMask =
                ACCESS_COLOR_ATTACHMENT_READ_BIT
                  .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            }
        ]
    }

newVulkanPipeline ::
  (MonadCatch m, MonadResource m) => Device -> RenderPass -> Extent2D -> m Pipeline
newVulkanPipeline device renderPass extent = do
  pipelineCreateInfo <- configureVulkanPipeline device renderPass extent
  logCreation $
    headThrow NoGraphicsQueue
      =<< vulkanResultCheck
        (withGraphicsPipelines device zero [SomeStruct pipelineCreateInfo] Nothing manageResource)

configureVulkanPipeline ::
  MonadResource m => Device -> RenderPass -> Extent2D -> m (GraphicsPipelineCreateInfo '[])
configureVulkanPipeline device renderPass extent@Extent2D {width = width, height = height} = do
  vertShaderModule <- withShaderModule device zero {code = snd vertShader} Nothing manageResource
  fragShaderModule <- withShaderModule device zero {code = snd fragShader} Nothing manageResource
  pipelineLayout <- withPipelineLayout device zero Nothing manageResource
  pure $
    zero
      { stages =
          [ SomeStruct
              zero
                { stage = SHADER_STAGE_VERTEX_BIT,
                  module' = vertShaderModule,
                  name = fst vertShader
                },
            SomeStruct
              zero
                { stage = SHADER_STAGE_FRAGMENT_BIT,
                  module' = fragShaderModule,
                  name = fst fragShader
                }
          ],
        vertexInputState = Just zero,
        inputAssemblyState =
          Just
            zero
              { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                primitiveRestartEnable = False
              },
        viewportState =
          Just $
            SomeStruct $
              zero
                { viewports =
                    [ Viewport
                        { x = 0,
                          y = 0,
                          width = fromIntegral width,
                          height = fromIntegral height,
                          minDepth = 0,
                          maxDepth = 1
                        }
                    ],
                  scissors = [Rect2D {offset = Offset2D {x = 0, y = 0}, extent = extent}]
                },
        rasterizationState =
          SomeStruct $
            zero
              { depthClampEnable = False,
                rasterizerDiscardEnable = False,
                lineWidth = 1,
                polygonMode = POLYGON_MODE_FILL,
                cullMode = CULL_MODE_NONE,
                frontFace = FRONT_FACE_CLOCKWISE,
                depthBiasEnable = False
              },
        multisampleState =
          Just $
            SomeStruct $
              zero
                { sampleShadingEnable = False,
                  rasterizationSamples = SAMPLE_COUNT_1_BIT,
                  minSampleShading = 1,
                  sampleMask = [maxBound]
                },
        depthStencilState = Nothing,
        colorBlendState =
          Just $
            SomeStruct $
              zero
                { logicOpEnable = False,
                  attachments =
                    [ zero
                        { colorWriteMask =
                            COLOR_COMPONENT_R_BIT
                              .|. COLOR_COMPONENT_G_BIT
                              .|. COLOR_COMPONENT_B_BIT
                              .|. COLOR_COMPONENT_A_BIT,
                          blendEnable = False
                        }
                    ]
                },
        dynamicState = Nothing,
        layout = pipelineLayout,
        renderPass = renderPass,
        subpass = 0,
        basePipelineHandle = zero
      }

newVulkanFramebuffer ::
  MonadResource m => Device -> RenderPass -> Extent2D -> ImageView -> m Framebuffer
newVulkanFramebuffer device renderPass extent imageView =
  let framebufferCreateInfo = configureVulkanFramebuffer device renderPass extent imageView
   in logCreation $ withFramebuffer device framebufferCreateInfo Nothing manageResource

configureVulkanFramebuffer ::
  Device -> RenderPass -> Extent2D -> ImageView -> FramebufferCreateInfo '[]
configureVulkanFramebuffer device renderPass Extent2D {width = width, height = height} imageView =
  zero
    { renderPass = renderPass,
      attachments = [imageView],
      width = width,
      height = height,
      layers = 1
    }

newVulkanCommandPool :: MonadResource m => Device -> Word32 -> m CommandPool
newVulkanCommandPool device graphicsQueueFamilyIndex =
  let commandPoolCreateInfo = configureVulkanCommandPool graphicsQueueFamilyIndex
   in logCreation $ withCommandPool device commandPoolCreateInfo Nothing manageResource

configureVulkanCommandPool :: Word32 -> CommandPoolCreateInfo
configureVulkanCommandPool graphicsQueueFamilyIndex =
  zero {queueFamilyIndex = graphicsQueueFamilyIndex}

newVulkanCommandBuffers ::
  MonadResource m =>
  Device ->
  RenderPass ->
  Pipeline ->
  Vector Framebuffer ->
  CommandPool ->
  Extent2D ->
  m (Vector CommandBuffer)
newVulkanCommandBuffers device renderPass pipeline framebuffers commandPool extent = do
  let commandBufferAllocateInfo = configureVulkanCommandBuffer framebuffers commandPool
  commandBuffers <- withCommandBuffers device commandBufferAllocateInfo manageResource
  logger "created CommandBuffers" $ commandBufferHandle <$> commandBuffers
  mapM_
    ( \(framebuffer, commandBuffer) ->
        useCommandBuffer commandBuffer zero { flags =COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT } $
          cmdUseRenderPass
            commandBuffer
            zero
              { renderPass = renderPass,
                framebuffer = framebuffer,
                renderArea = Rect2D {offset = zero, extent = extent},
                clearValues = [Color (Float32 0.1 0.1 0.1 0)]
              }
            SUBPASS_CONTENTS_INLINE
            $ do
              cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
              cmdDraw commandBuffer 3 1 0 0
    )
    $ Vector.zip framebuffers commandBuffers
  pure commandBuffers

configureVulkanCommandBuffer ::
  Vector Framebuffer -> CommandPool -> CommandBufferAllocateInfo
configureVulkanCommandBuffer framebuffers commandPool =
  zero
    { commandPool = commandPool,
      level = COMMAND_BUFFER_LEVEL_PRIMARY,
      commandBufferCount = fromIntegral $ length framebuffers
    }

-- sdl
initSDL :: MonadResource m => m ()
initSDL =
  manageResource_
    (SDL.initialize @[] [SDL.InitVideo, SDL.InitEvents])
    SDL.quit

newSDLWindow ::
  (MonadResource m, MonadIO m) =>
  Text ->
  SDL.WindowConfig ->
  m SDL.Window
newSDLWindow title config = do
  manageResource
    (SDL.createWindow title config)
    SDL.destroyWindow

-- -- やっぱなし ~TODO: 最後にloopを有効化~
-- -- sdlLoop $ windowAction window
-- windowAction window

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
    (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr $ instanceHandle vulkanInstance))
    $ \surface -> destroySurfaceKHR vulkanInstance surface Nothing

-- utility
instance Exception Result

data Error
  = MissingRequiredExtensions (Vector ByteString)
  | MissingRequiredLayers (Vector ByteString)
  | MissingRequiredDeviceExtensions (Vector ByteString)
  | MissingRequiredPhysicalDeviceExtensions (Vector ByteString)
  | MissingDesiredSurfaceFormat (Vector SurfaceFormatKHR)
  | MissingDesiredPresentMode (Vector PresentModeKHR)
  | NoPhysicalDeviceWithRequiredExtensions (Vector ByteString)
  | NoGraphicsQueue
  | NoPresentQueue
  | NoGraphicsPipeline
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
  (Vector a -> e) ->
  Vector a ->
  Vector a ->
  m ()
checkRequired wrap available required =
  let missing = getMissing available required
   in if null missing
        then pure ()
        else throw $ wrap missing

getMissing :: Eq a => Vector a -> Vector a -> Vector a
getMissing available =
  Vector.filter (`notElem` available)

isFlagged :: Bits a => a -> a -> Bool
isFlagged flagBit flag = zeroBits /= flagBit .&. flag

stringConvert :: MonadIO m => CString -> m ByteString
stringConvert = liftIO . BS.packCString

logger :: (MonadIO m, Show a) => String -> a -> m ()
logger message =
  liftIO . (*>) (putStr (message <> " ")) . print

logCreation ::
  (MonadIO m, Show a) => m a -> m a
logCreation creator = do
  created <- creator
  logger "created" created
  pure created

manageResource ::
  MonadResource m => IO a -> (a -> IO ()) -> m a
-- manageResource alloc free = managed (bracket alloc free)
manageResource alloc free = snd <$> allocate alloc free

manageResource_ :: MonadResource m => IO a -> IO () -> m a
manageResource_ alloc = manageResource alloc . const
