-module(otc_codec).

-callback spec() -> string().

-callback codec(binary() | map() | tuple()) -> map() | binary().

-callback next(map()) -> '$stop' | {ok, atom()}.
