open Prelude

signature Input = sig
    type data
    type group
    type member
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
end

functor Make(M : Input) = struct

open M

type connection = channel data

table listeners :
    {Group : group,
     Member : member,
     Channel : channel data}
    PRIMARY KEY (Group, Member)

fun tell group data =
    queryI1 (Sql.selectLookup listeners {Group = group})
            (fn {Channel = chan} => send chan data)

fun connect user =
    chan <- channel;
    Sql.insert listeners (user ++ {Channel = chan});
    return chan

val listen = spawnListener

end
