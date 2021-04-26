module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate, allocate_, runResourceT)
import Data.String (IsString)
import Data.Text (Text)
import qualified SDL (Event (Event, eventPayload), EventPayload (KeyboardEvent, QuitEvent), InitFlag (InitEvents, InitVideo), InputMotion (Pressed), KeyboardEventData (KeyboardEventData, keyboardEventKeyMotion, keyboardEventKeysym), Keysym (Keysym, keysymKeycode), V2 (..), Window, WindowConfig (WindowConfig, windowGraphicsContext, windowInitialSize), WindowGraphicsContext (VulkanContext), createWindow, defaultWindow, destroyWindow, initialize, pollEvents, quit)
import qualified SDL.Input (pattern KeycodeQ)
import qualified SDL.Video.Vulkan as SDL (vkLoadLibrary, vkUnloadLibrary)
import Vulkan.Core10 (SpecializationInfo (data'), Viewport (height, width), enumerateInstanceExtensionProperties, enumerateInstanceLayerProperties, withInstance)
import Vulkan.Zero (zero)

appName :: IsString a => a
appName = "Haskell Vulkan triangle example"

windowDimention :: (Int, Int) -- (width, height)
windowDimention = (800, 600)

main :: IO ()
-- Create an instance and print its value
main = runResourceT $ do
  initSDL

  -- (instanceReleaseKey, inst) <- withInstance zero Nothing allocate
  -- (_, layers) <- enumerateInstanceLayerProperties
  -- (_, extentions) <- enumerateInstanceExtensionProperties Nothing
  -- liftIO $ print inst *> print layers *> print extentions

  window <- newWindow appName windowDimention
  windowLoop window $ const $ pure ()
  drawTriangle

newWindow ::
  (MonadResource m, MonadIO m) =>
  Text ->
  (Int, Int) ->
  m SDL.Window
newWindow title (width, height) =
  snd
    <$> allocate
      ( SDL.createWindow
          title
          SDL.defaultWindow
            { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height),
              SDL.windowGraphicsContext = SDL.VulkanContext
            }
      )
      SDL.destroyWindow

windowLoop ::
  MonadIO m =>
  SDL.Window ->
  (SDL.Window -> m ()) ->
  m ()
windowLoop window action = do
  willQuit <- any isQ <$> SDL.pollEvents
  unless willQuit $ do
    action window
    windowLoop window action
  where
    isQ SDL.Event {SDL.eventPayload = SDL.QuitEvent} = True
    isQ event = False

initSDL :: (MonadResource m, MonadIO m) => m ()
initSDL = do
  allocate_ (SDL.initialize [SDL.InitVideo, SDL.InitEvents]) SDL.quit
  allocate_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
  pure ()

drawTriangle :: (MonadResource m, MonadIO m) => m ()
drawTriangle = do
  pure ()

-- data VulkanWindow = VulkanWindow
--   { vwSDLWindow :: SDL.Window
--   }

-- newVulkanWindow ::
--   (MonadManaged m, MonadIO m) =>
--   (MonadResource m, MonadIO m) =>
--   (Int, Int) ->
--   m VulkanWindow
-- newVulkanWindow title dimention = do
--   window <- newWindow title dimention
--   pure VulkanWindow {vwSDLWindow = window}
