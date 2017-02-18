structure Posts = MagicTable.Make(struct
    con chan = #Channel
    val label_fields =
        {Title = "Title",
         Author = "Author",
         Body = "Body",
         Date = "Date",
         Time = "Time"}
end)
