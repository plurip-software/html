module Types where

import qualified Path as P
import Data.List.Split (splitOneOf)

class Parser a where
    stringify :: a -> String
    parse     :: String -> Maybe a

class Show a => Encloseable a where
    enclose :: a -> String

newtype Code = Code String

instance Show Code where
    show (Code code) = code

data HTML = HTML [Element] -- Should be Doctype -> HTML -> Head -> Body

instance Show HTML where
    show (HTML elements) = show $ foldl (\acc element -> acc ++ show element) "" elements 

data Element 
    = Element Tag Attributes Children 
    | ElementNoEnd Tag Attributes
    | ElementDoctypeHtml

instance Show Element where
    show (Element tag attributes children) = "<" ++ show tag ++ " " ++ show attributes ++ ">" ++ show children ++ "</" ++ show tag ++ ">"
    show (ElementNoEnd tag attributes) = "<" ++ show tag ++ " " ++ show attributes ++ "/>"
    show ElementDoctypeHtml = "<!DOCTYPE html>"

instance Show attributes => Show (Element attributes) where
    show (Element tag attributes children) = show tag  

data Tag 
    = Div
    | Html
    | Head
    | Body
    | Title
    | P
    | Meta
    | Script
    | Input

instance Show Tag where
    show Div         = "div"  
    show Html        = "html"  
    show Head        = "head"  
    show Body        = "body" 
    show Title       = "title"  
    show Meta        = "meta"  
    show P           = "p"  
    show Script      = "script"  
    show Input       = "input"  

instance Parser Tag where
    stringify = show
    parse "div"             = Just Div   
    parse "html"            = Just Html  
    parse "head"            = Just Head  
    parse "body"            = Just Body  
    parse "title"           = Just Title 
    parse "meta"            = Just Meta  
    parse "p"               = Just P     
    parse "script"          = Just Script
    parse "input"           = Just Input 
    parse _                 = Nothing

newtype Children = Children [Element] 

instance Show Children where
    show (Children children) = foldl (\acc child -> acc ++ show child) "" children

instance Parser Children where
    stringify = show
    parse childrenStr =
        splitOneOf "<>" childrenStr 
        

data Charset
    = Windows_1252
    | ISO_8859
    | UTF_8
    | UTF_16

instance Show Charset where
    show Windows_1252 = "charset='Windows-1252' "
    show ISO_8859 = "charset='ISO-8859' "
    show UTF_8   = "charset='UTF-8' "
    show UTF_16  = "charset='UTF-16' "

data InputType 
    = Button
    | Checkbox
    | Color
    | Date
    | Datetimeocal
    | Email
    | File
    | Hidden
    | Image
    | Month
    | Number
    | Password
    | Radio
    | Range
    | Reset
    | Search
    | Submit
    | Tel
    | Text
    | Time
    | Url

instance Show InputType where
    show Button = "type='button' "
    show Checkbox = "type='checkbox' "
    show Color = "type='color' "
    show Date = "type='date' "
    show Datetimeocal = "type='datetimeocal' "
    show Email = "type='email' "
    show File = "type='file' "
    show Hidden = "type='hidden' "
    show Image = "type='image' "
    show Month = "type='month' "
    show Password = "type='password' "
    show Radio = "type='radio' "
    show Range = "type='range' "
    show Reset = "type='reset' "
    show Search = "type='search' "
    show Submit = "type='submit' "
    show Tel = "type='tel' "
    show Text = "type='text' "
    show Time = "type='time' "
    show Url = "type='url' "

newtype Attributes = Attributes [Attribute]

instance Show Attributes where
    show (Attributes attributes) = intercalate " " $ map show attributes

data Attribute
    = HttpEqiv String
    | Value String
    | Content String
    | ID String
    | Class String
    | Classes String
    | Name String
    | Style CSS
    | Src P.Path

instance Show Attribute where
    show (HttpEqiv httpEqiv) = "http-eqiv='" ++ httpEqiv ++ "' "
    show (Value_ value_) = "value='" ++ value_ ++ "' "
    show (Content content) = "content='" ++ content ++ "' "
    show (ID id) = "id='" ++ id ++ "' "
    show (Class class) = "class='" ++ class ++ "' "
    show (Name name) = "name='" ++ name ++ "' "
    show (Style css) = "style='" ++  show css ++ "' "
    show (Src path) = "src='" ++ show path ++ "' "

newtype CSS = CSS [(Key, Value)]

instance Show CSS where
    show (CSS css) = foldl (\acc (key, value) -> acc ++ show key ++ ":" ++ "'" ++ show value_ ++ "';") "" css

instance Parser CSS where
    stringify = show
    parse = CSS . map keyValuePair . map splitOn ":" . endBy ";"  

keyValuePair :: [String, String] -> (Key, Value)   
keyValuePair [key, value] = (Key key, Value value)

newtype Key = Key String

instance Show Key where
    show (Key key) = key

newtype Value = Value String

instance Show Value_ where
    show (Value value) = value