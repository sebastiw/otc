-module(otc_codec).

-callback spec() -> string().

-type part() :: {map(), binary()}.
-type options() :: map().
-callback codec(binary() | map() | part(), options()) -> map() | binary().

-callback next(map()) -> '$stop' | {ok, atom()}.

-callback propagated_options(map(), options()) -> options().
-optional_callbacks([propagated_options/2]).
