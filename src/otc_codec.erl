-module(otc_codec).

-callback spec() -> string().

-type part() :: {map(), binary()}.
-callback codec(binary() | map() | part()) -> map() | binary().

-callback next(map()) -> '$stop' | {ok, atom()}.
