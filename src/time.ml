open CalendarLib

let format_timeout time =
  Printer.Fcalendar.to_string (Fcalendar.from_unixfloat time)
let current_string () =
  Printer.Fcalendar.sprint "%s" (Fcalendar.now ())
