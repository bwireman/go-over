-module(yamll).

-export([
    parse/1
]).


parse(Path) -> 
    [Content] = yamerl:decode_file(Path),
    Name = lists:keyfind("package", 1, Content),
    Vulnerable_version_ranges = lists:keyfind("vulnerable_version_ranges", 1, Content),
    {Name, Vulnerable_version_ranges}.