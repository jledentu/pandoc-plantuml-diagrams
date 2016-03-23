
-- | Module : Text.Pandoc.PlantUML.Filter.OutputBlock
-- Renders an image file name and some attributes into a Pandoc
-- block, like so:
--
-- @
-- Para
--   Image src=picture.jpg
--   "{#fig:id}"
-- @
module Text.Pandoc.PlantUML.Filter.OutputBlock(resultBlock) where

import Text.Pandoc.JSON
import Text.Pandoc.PlantUML.Filter.Types
import Data.Maybe

-- | The result block, as specified in the module header.
resultBlock :: ImageFileName -> Attr -> Block
resultBlock imageFileName attr = Para [imageTag imageFileName attr]

imageTag :: ImageFileName -> Attr -> Inline
imageTag imageFileName attr    = Image attr (altTagInline attr) ((show imageFileName), "fig:")

altTagInline :: Attr -> [Inline]
altTagInline (_, _, keyValues)
  | isJust altText             = [Str (fromJust altText)]
  | otherwise                  = []
  where altText = lookup "caption" keyValues
