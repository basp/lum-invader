%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @doc Command parser.
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_ansi).

-export([style/1, strip/1]).

-define(RESET, 				<<27, $[, $0, $m>>).

-define(BOLD,				<<27, $[, $1, $m>>).
-define(ITALICS,			<<27, $[, $3, $m>>).
-define(UNDERLINE, 			<<27, $[, $4, $m>>).
-define(INVERSE,			<<27, $[, $7, $m>>).
-define(STRIKETHROUGH, 		<<27, $[, $9, $m>>).

-define(BOLD_OFF, 			<<27, $[, $2, $2, $m>>).
-define(ITALICS_OFF,		<<27, $[, $2, $3, $m>>).
-define(UNDERLINE_OFF,		<<27, $[, $2, $4, $m>>).
-define(INVERSE_OFF, 		<<27, $[, $2, $7, $m>>).
-define(STRIKETHROUGH_OFF, 	<<27, $[, $2, $9, $m>>).

-define(FG_BLACK,			<<27, $[, $3, $0, $m>>).
-define(FG_RED,				<<27, $[, $3, $1, $m>>).
-define(FG_GREEN,			<<27, $[, $3, $2, $m>>).
-define(FG_YELLOW,			<<27, $[, $3, $3, $m>>).
-define(FG_BLUE,			<<27, $[, $3, $4, $m>>).
-define(FG_MAGENTA,			<<27, $[, $3, $5, $m>>).
-define(FG_CYAN,			<<27, $[, $3, $6, $m>>).
-define(FG_WHITE,			<<27, $[, $3, $7, $m>>).
-define(FG_DEFAULT,			<<27, $[, $3, $9, $m>>).

-define(BG_BLACK,			<<27, $[, $4, $0, $m>>).
-define(BG_RED,				<<27, $[, $4, $1, $m>>).
-define(BG_GREEN,			<<27, $[, $4, $2, $m>>).
-define(BG_YELLOW,			<<27, $[, $4, $3, $m>>).
-define(BG_BLUE,			<<27, $[, $4, $4, $m>>).
-define(BG_MAGENTA,			<<27, $[, $4, $5, $m>>).
-define(BG_CYAN,			<<27, $[, $4, $6, $m>>).
-define(BG_WHITE,			<<27, $[, $4, $7, $m>>).
-define(BG_DEFAULT,			<<27, $[, $4, $9, $m>>).

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Applies ANSI color sequences.
-spec style(Data::binary()) -> binary().
style(Data) ->
	style(Data, <<>>).

%% @doc Strips all style info.
-spec strip(Data::binary()) -> binary().
strip(Data) ->
	strip(Data, <<>>).

%%%============================================================================
%%% Internal functions
%%%============================================================================

style(<<>>, Acc) -> Acc;
style(Data, Acc) ->
	case Data of
		<<"$reset;", Rest/binary>> -> 
			style(Rest, <<Acc/binary, ?RESET/binary>>);
		<<"$bold;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BOLD/binary>>);
		<<"$italics;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?ITALICS/binary>>);
		<<"$underline;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?UNDERLINE/binary>>);
		<<"$inverse;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?INVERSE/binary>>);
		<<"$strikethrough;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?STRIKETHROUGH/binary>>);
		<<"$bold_off;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BOLD_OFF/binary>>);
		<<"$italics_off;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?ITALICS_OFF/binary>>);
		<<"$underline_off;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?UNDERLINE_OFF/binary>>);
		<<"$inverse_off;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?INVERSE_OFF/binary>>);
		<<"$strikethrough_off;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?STRIKETHROUGH_OFF/binary>>);
		<<"$fg_black;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_BLACK/binary>>);
		<<"$fg_red;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_RED/binary>>);
		<<"$fg_green;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_GREEN/binary>>);
		<<"$fg_yellow;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_YELLOW/binary>>);
		<<"$fg_blue;", Rest/binary>> -> 
			style(Rest, <<Acc/binary, ?FG_BLUE/binary>>);
		<<"$fg_magenta;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_MAGENTA/binary>>);
		<<"$fg_cyan;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_CYAN/binary>>);
		<<"$fg_white;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_WHITE/binary>>);
		<<"$fg_default;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?FG_DEFAULT/binary>>);
		<<"$bg_black;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_BLACK/binary>>);
		<<"$bg_red;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_RED/binary>>);
		<<"$bg_green;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_GREEN/binary>>);
		<<"$bg_yellow;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_YELLOW/binary>>);
		<<"$bg_blue;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_BLUE/binary>>);
		<<"$bg_magenta;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_MAGENTA/binary>>);
		<<"$bg_cyan;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_CYAN/binary>>);
		<<"$bg_white;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_WHITE/binary>>);
		<<"$bg_default;", Rest/binary>> ->
			style(Rest, <<Acc/binary, ?BG_DEFAULT/binary>>);
		<<C, Rest/binary>> ->
			style(Rest, <<Acc/binary, C>>)
	end.

strip(<<>>, Acc) -> Acc;
strip(Data, Acc) ->
	case Data of
		<<"$reset;", Rest/binary>>                -> strip(Rest, Acc);
		<<"$bold;", Rest/binary>>                 -> strip(Rest, Acc);
		<<"$italics;", Rest/binary>>              -> strip(Rest, Acc);
		<<"$underline;", Rest/binary>>            -> strip(Rest, Acc);
		<<"$inverse;", Rest/binary>>              -> strip(Rest, Acc);
		<<"$strikethrough;", Rest/binary>>        -> strip(Rest, Acc);
		<<"$bold_off;", Rest/binary>>             -> strip(Rest, Acc);
		<<"$italics_off;", Rest/binary>>          -> strip(Rest, Acc);
		<<"$underline_off;", Rest/binary>>        -> strip(Rest, Acc);
		<<"$inverse_off;", Rest/binary>>          -> strip(Rest, Acc);
		<<"$strikethrough_off;", Rest/binary>>    -> strip(Rest, Acc);
		<<"$fg_black;", Rest/binary>>             -> strip(Rest, Acc);
		<<"$fg_red;", Rest/binary>>               -> strip(Rest, Acc);
		<<"$fg_green;", Rest/binary>>             -> strip(Rest, Acc);
		<<"$fg_yellow;", Rest/binary>>            -> strip(Rest, Acc);
		<<"$fg_blue;", Rest/binary>>              -> strip(Rest, Acc);
		<<"$fg_magenta;", Rest/binary>>           -> strip(Rest, Acc);
		<<"$fg_cyan;", Rest/binary>>              -> strip(Rest, Acc);
		<<"$fg_white;", Rest/binary>>             -> strip(Rest, Acc);
		<<"$fg_default;", Rest/binary>>           -> strip(Rest, Acc);
		<<"$bg_black;", Rest/binary>>             -> strip(Rest, Acc);
		<<"$bg_red;", Rest/binary>>               -> strip(Rest, Acc);
		<<"$bg_green;", Rest/binary>>             -> strip(Rest, Acc);
		<<"$bg_yellow;", Rest/binary>>            -> strip(Rest, Acc);
		<<"$bg_blue;", Rest/binary>>              -> strip(Rest, Acc);
		<<"$bg_magenta;", Rest/binary>>           -> strip(Rest, Acc);
		<<"$bg_cyan;", Rest/binary>>              -> strip(Rest, Acc);
		<<"$bg_white;", Rest/binary>>             -> strip(Rest, Acc);
		<<"$bg_default;", Rest/binary>>           -> strip(Rest, Acc);
		<<C, Rest/binary>> ->
			strip(Rest, <<Acc/binary, C>>)
	end.