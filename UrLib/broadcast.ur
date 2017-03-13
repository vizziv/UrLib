open Prelude

signature Types = sig
    type data
    type group
    type member
end

signature Input = sig
    include Types
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
end

signature Output = sig
    include Types
    val tell : group -> data -> tunit
    type connection
    val connect : {Group : group, Member : member} -> transaction connection
    val listen : (data -> tunit) -> connection -> tunit
end

functor Make(M : Input) : Output
    where type data = M.data
    where type group = M.group
    where type member = M.member = struct

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
