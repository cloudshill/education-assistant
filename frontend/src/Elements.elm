module Elements exposing (primaryButton, primaryButtonScaled, secondaryButton)

import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Palette


primaryButtonScaled : String -> msg -> Float -> E.Element msg
primaryButtonScaled msg onClick scale =
    Input.button [ E.width <| E.px <| round <| toFloat 250 * scale, E.height <| E.px <| round <| toFloat 45 * scale, E.centerX, Border.width 1, Border.rounded 12, Border.color Palette.primary, Background.color Palette.primary ]
        { onPress = Just onClick
        , label = E.el [ E.centerX, E.centerY ] <| E.text msg
        }


primaryButton msg onClick =
    primaryButtonScaled msg onClick 1.0


secondaryButton msg onClick =
    Input.button [ E.width <| E.px 250, E.height <| E.px 45, Border.width 1, Border.rounded 12, Border.color Palette.primary, E.centerX ]
        { onPress = Just onClick
        , label = E.el [ E.centerX, E.centerY ] <| E.text msg
        }
