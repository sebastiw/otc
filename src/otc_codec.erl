-module(otc_codec).

-callback spec() -> string().

-type record() :: tuple().
-callback codec(binary() | map() | record()) -> map() | binary().

-callback next(map()) -> '$stop' | {ok, atom()}.
