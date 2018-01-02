type t = int

cookie user : t

table users : {User : t, Pulse : time}

val refresh = 3600 (* 1 hour in seconds *)
val timeout = 2 * refresh

task periodic refresh = fn () =>
    n <- now;
    queryI1 (SELECT * FROM users)
            (fn {User = u, Pulse = p} =>
                if diffInSeconds p n > timeout then
                    Sql.deleteLookup users {User = u}
                else
                    return ())

val newUser =
    u <- rand;
    p <- now;
    Sql.insert users {User = u, Pulse = p};
    return u

fun verify uq =
    case uq of
        None => newUser
      | Some u =>
        pq <- oneOrNoRows1 (Sql.selectLookup users {User = u});
        case pq of
            None => newUser
          | Some {Pulse = p} =>
            n <- now;
            (if diffInSeconds p n > refresh then
                 Sql.updateLookup users {User = u} {Pulse = n}
             else
                 return ());
            return u

val get = bind (getCookie user) verify
