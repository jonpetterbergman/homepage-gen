module HomepageGen.Html.Template where

import HomepageGen.Data.Site       (Lang)
import HomepageGen.Data.Navigation (Navigation)
import Text.Blaze.Html             (Html)

type Template =  (Lang,[Lang],Navigation)
              -> Html