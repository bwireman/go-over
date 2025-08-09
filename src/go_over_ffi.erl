-module(go_over_ffi).

-export([
    parse_adv/1
]).

% Parses mirego/elixir-security-advisories advisory yaml
% files and returns these fields in an array
% - id: binary()
% - package: binary()
% - severity: binary()
% - title: binary()
% - vulnerable_version_ranges: list(binary())
%
% @param {binary()} content
% @returns {ok, {binary(), binary(), binary(), binary(), list(binary())}}
parse_adv(Raw) ->
    try do_parse_adv(Raw) of
        Res -> Res
    catch
        _:_ -> {error, nil}
    end.

do_parse_adv(Raw) ->
    maybe
        [Content] ?= yamerl:decode(Raw),

        {_, ID_raw} ?= lists:keyfind("id", 1, Content),
        ID ?= unicode:characters_to_binary(ID_raw),
        true ?= is_binary(ID),

        {_, Name_raw} ?= lists:keyfind("package", 1, Content),
        Name ?= unicode:characters_to_binary(Name_raw),
        true ?= is_binary(Name),

        {_, Severity_raw} ?= lists:keyfind("severity", 1, Content),
        Severity ?= unicode:characters_to_binary(Severity_raw),
        true ?= is_binary(Severity),

        {_, Title_raw} ?= lists:keyfind("title", 1, Content),
        Title ?= unicode:characters_to_binary(Title_raw),
        true ?= is_binary(Title),

        {_, Vulnerable_version_ranges_raw} ?= lists:keyfind("vulnerable_version_ranges", 1, Content),
        Vulnerable_version_ranges ?= lists:map(
            fun unicode:characters_to_binary/1, Vulnerable_version_ranges_raw
        ),
        true ?= lists:all(fun is_binary/1, Vulnerable_version_ranges),

        {ok, {
            ID,
            Name,
            Severity,
            Title,
            Vulnerable_version_ranges
        }}
    else
        _ -> {error, nil}
    end.


    
