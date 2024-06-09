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
% @returns {binary(), binary(), binary(), binary(), list(binary())}
parse_adv(Raw) ->
    [Content] = yamerl:decode(Raw),
    {_, ID} = lists:keyfind("id", 1, Content),
    {_, Name} = lists:keyfind("package", 1, Content),
    {_, Severity} = lists:keyfind("severity", 1, Content),
    {_, Title} = lists:keyfind("title", 1, Content),
    {_, Vulnerable_version_ranges} = lists:keyfind("vulnerable_version_ranges", 1, Content),
    {
        unicode:characters_to_binary(ID),
        unicode:characters_to_binary(Name),
        unicode:characters_to_binary(Severity),
        unicode:characters_to_binary(Title),
        lists:map(fun unicode:characters_to_binary/1, Vulnerable_version_ranges)
    }.
