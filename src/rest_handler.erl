-module(rest_handler).

-callback init() -> atom().

-callback match(atom(), string(), tuple() |
                atom(), [{atom(), atom()}]) -> tuple().
