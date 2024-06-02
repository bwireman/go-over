-module(advisory_yaml).

-export([
    parse/1
]).

parse(Path) -> 
    [Content] = yamerl:decode_file(Path),
    {_, ID} = lists:keyfind("id", 1, Content),
    {_, Name} = lists:keyfind("package", 1, Content),
    {_, Severity} = lists:keyfind("severity", 1, Content),
    {_, Title} = lists:keyfind("title", 1, Content),
    {_, Vulnerable_version_ranges} = lists:keyfind("vulnerable_version_ranges", 1, Content),
    {ID, Name, Severity, Title, Vulnerable_version_ranges}.