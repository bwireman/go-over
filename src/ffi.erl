-module(ffi).

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
        {_, ID} ?= lists:keyfind("id", 1, Content),
        {_, Name} ?= lists:keyfind("package", 1, Content),
        {_, Severity} ?= lists:keyfind("severity", 1, Content),
        {_, Title} ?= lists:keyfind("title", 1, Content),
        {_, Vulnerable_version_ranges} ?= lists:keyfind("vulnerable_version_ranges", 1, Content),
        ID_parsed ?= unicode:characters_to_binary(ID),
        Name_parsed ?= unicode:characters_to_binary(Name),
        Severity_parsed ?= unicode:characters_to_binary(Severity),
        Title_parsed ?= unicode:characters_to_binary(Title),
        Vulnerable_version_ranges_parsed ?= lists:map(
            fun unicode:characters_to_binary/1, Vulnerable_version_ranges
        ),
        true ?= is_binary(ID_parsed),
        true ?= is_binary(Name_parsed),
        true ?= is_binary(Severity_parsed),
        true ?= is_binary(Title_parsed),
        true ?= lists:all(fun is_binary/1, Vulnerable_version_ranges_parsed),
        {ok, {
            ID_parsed,
            Name_parsed,
            Severity_parsed,
            Title_parsed,
            Vulnerable_version_ranges_parsed
        }}
    else
        _ -> {error, nil}
    end.


    
