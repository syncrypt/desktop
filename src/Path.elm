module Path
    exposing
        ( Path
        , asPath
        , folderName
        , fromString
        , inRoot
        , name
        , parent
        , toString
        )

import Util


type alias Path =
    List String


folderName : Path -> String
folderName path =
    case path of
        [] ->
            "./"

        [ x ] ->
            x

        _ :: rest ->
            folderName rest


name : Path -> String
name path =
    case path of
        [] ->
            "."

        [ f ] ->
            f

        _ :: rest ->
            name rest


asPath : String -> Path
asPath pathString =
    if String.startsWith "/" pathString then
        String.split "/" pathString
    else
        String.split "\\" pathString


inRoot : Path -> Bool
inRoot path =
    List.length path == 1


parent : Path -> Path
parent path =
    path
        |> Util.allButLast


toString : String -> Path -> String
toString pathSeparator path =
    String.join pathSeparator path


fromString : String -> String -> Path
fromString pathSep pathStr =
    String.split pathSep pathStr
