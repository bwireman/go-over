-module(yamll).

-export([
    parse/1
]).

parse(Path) -> 
    [Content] = yamerl:decode_file(Path),
    {_, Name} = lists:keyfind("package", 1, Content),
    {_, Severity} = lists:keyfind("severity", 1, Content),
    {_, Desc} = lists:keyfind("description", 1, Content),
    {_, Vulnerable_version_ranges} = lists:keyfind("vulnerable_version_ranges", 1, Content),
    {Name, Severity, Desc, Vulnerable_version_ranges}.