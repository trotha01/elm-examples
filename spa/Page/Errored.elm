module Page.Errored exposing (PageLoadError)


type PageLoadError
    = PageLoadError Model


type alias Model =
    { errorMessage : String
    }
