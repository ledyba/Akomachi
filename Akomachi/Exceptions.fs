namespace Akomachi
module Exceptions=
    exception ParseException of (string)
    exception InvalidArgumentException of (string)
    exception TypeMismatchException of (System.Type * string)
    exception InvalidCastException of (string)
    exception UnsupportedBoxingException of (System.Type * string)
    exception UnsupportedUnboxingException of (System.Type * string)
    exception InvalidNativeObjectException of (System.Type * string)
    exception BrokenSavedataException of (string)
    exception PropertyNotFoundException of (string * string)
    exception InvalidInvocationException of (string)
    exception InvalidAccessingException of (string)
