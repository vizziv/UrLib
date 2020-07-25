open Prelude

signature Input = sig
    type message
    type group
    type member
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
end

functor Make(M : Input) = struct

open M

type connection = channel message

table listeners :
    {Group : group,
     Member : member,
     Channel : channel message}
    PRIMARY KEY (Group, Member)

fun tell group message =
    queryI1 (Sql.selectLookup listeners {Group = group})
            (fn {Channel = chan} => send chan message)

fun connect user =
    chan <- channel;
    Sql.insert listeners (user ++ {Channel = chan});
    return chan

val listen = spawnListener

end
