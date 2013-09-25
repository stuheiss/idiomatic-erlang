Module Design
-------------

Module design really is in the hands of the designer. Outside of a few
good rules of thumb and some default organizational strategies there
aren't to many ways to get yourself in trouble.


Organizing your modules is pretty strait forward. Mostly it involves
separating your modules into sections and documenting each section
well. Let me give you an example of a real module in use in production
software.

```erlang

%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  A group of utility functions for project automation.
%%% @end
%%% @copyright (C) 2007, Erlware
%%%---------------------------------------------------------------------------
-module(sin_utils).

-include("file.hrl").

%% API
-export([copy_dir/3,
         copy_dir/4,
         parent_dir/1,
         delete_dir/1,
         remove_code_paths/1,
         is_dir_ignorable/2,
         file_exists/1]).


%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%  Check to see if a file exists.
%%
%% @spec (FileName) -> true | false
%% @end
%%-------------------------------------------------------------------
file_exists(FileName) ->
    case file:read_file_info(FileName) of
        {error, enoent} ->
            false;
        Error = {error, _} ->
            throw(Error);
        _ ->
            true
    end.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Check to see if the file is a symlink.
%% @spec (Name) -> true | false
%% @end
%%--------------------------------------------------------------------
is_symlink(Name) ->
    case catch file:read_link_info(Name) of
        {ok, Env} ->
            Env#file_info.type == symlink;
        _Else ->
            false
    end.

```

The module presented here is divided into several sections. Generally
there is a Header area. This is followed by an API section that
contains the functions that make up the public API of your
module. Next is the Internal Functions section. This area comprises
functions that are required by your module but are not part of its
public API and are not exported. Finally comes the Test section where
your Eunit tests will be placed. None of these sections are required
by the language. It has, however, become pretty a pretty common way to
organize modules. I like it and use it. I suggest you do as well.

Lets get into some detail on the organization. First notice the
header. It uses Edoc formated comments to describe the file. Edoc
comments allow documentation to be automatically generated. This is
equivalent to Javadocs for the Java system. You will notice that both
the header and all of the function comments use Edoc documentation in
this way. Following the header in quick succession are module
attribute, any imports that are required by the module and the export
attribute that details the public functions in your module. Its pretty
common to split exports for your API functions with exports that you
may need for the internals of an application you are
building. Following the export attributes are any macro definitions
and record definitions that you may need. This header area is then
followed by the sections we described previously. As I have said none
of this is a hard requirement but it will help you provide a
consistent look and feel for your modules and reduce the maintenance
burden by just a bit. Especially for someone coming into the code base
after you.
