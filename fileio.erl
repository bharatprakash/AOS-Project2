-module(fileio).

-export([read_n_lines/2]).

read_n_lines(Filename,NumLines) ->
     {ok, FileDev} = file:open(Filename, [raw, read, read_ahead]),
     Lines = do_read([],FileDev, NumLines),
     file:close(FileDev),
     Lines.

do_read(Lines, _, 0) ->
     lists:reverse(Lines);
do_read(Lines, FileDev, L) ->
     case file:read_line(FileDev) of
          {ok, Line} ->
			   %%io:format("~p ~n", [Lines
               do_read([Line|Lines], FileDev, L - 1);
          eof ->
               do_read(Lines, FileDev, 0)
     end.
	 
	 