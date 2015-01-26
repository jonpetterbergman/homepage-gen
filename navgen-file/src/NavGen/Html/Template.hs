module NavGen.Html.Template where

import NavGen.Data.Site       (Lang)
import NavGen.Data.Navigation (Navigation)
import Text.Blaze.Html        (Html)

type Template = (Lang,[Lang],Navigation Html)
              -> Html