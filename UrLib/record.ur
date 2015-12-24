fun set [const] [modify] [insert]
        [const ~ modify] [const ~ insert] [modify ~ insert]
        (xs : $(const ++ modify)) (ys : $(modify ++ insert))
        : $(const ++ modify ++ insert) =
    xs --- modify ++ ys
