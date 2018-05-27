# Covella
'Juno Covella' is the name the ancient Roman 'King of Sacrifices' called at the beginning of each month.

This package provides a DSL for arbitrary systems of time keeping.
- Create your own time units and calendar.
- Combine several cycles into one calendar. (Yes, it can do moons.)
- Create immutable dates.
- Define date formats and parsers in a simple manner.
- Create queries like 'first Tuesday in November' using streams.

## How to

### Your first calendar
The most common calendars use years composed of months composed of days, the months having a varying number of days and names. If that is all you need, you can create your first calendar like this:
- Extend trait `MonthsAndYears`.
- Provide a value called `days : TimeUnit`. If you are looking for days of 24 hours of 60 minutes of 60 seconds, you can simply use:

    val days : TimeUnit = CommonDays.days
    
You can then use the `year` method to create your year like so:

    val standardYear : TimeUnit =  year(
    (31, "January"),
    (28, "February"),
    (31, "March"),
    (30, "April"),
    (31, "May"),
    (30, "June"),
    (31, "July"),
    (31, "August"),
    (30, "September"),
    (31, "October"),
    (30, "November"),
    (31, "December"))


### Calendars and Eras
Now, let's turn it into a calendar. 

    val simpleCalendar = Calendar(standardYear)
    
That will work, but now all years are the same. To amend that first attempt, we first need a `leapYear`. We can construct it like the standard year but with a different Feburary. Then we need a way to define our leap rule or "intercalation", if you feel fancy.
 
     val moreAdvancedEra = Era given divisibleBy(4) have leapYear default standardYear
     
An Era instance works like partial function. You can add a condition with `given` followed by `have` and the `TimeUnit` you want. You can add any number of such pairs. While any predicate of BigInt is possible after `given`, it is much better to use functions implementing the `PeriodicFunction`interface, because then the Calendar will automatically optimise.

Once you are done, you may use `default` to set a default. Then put the era into a Calendar.

     val moreAdvancedCalendar = Calendar(moreAdvancedEra)

Note that this is not really the calendar we use. The leap rule is more complicated, there is no year 0, and things are really weird in 1582. If you want to savor the thing, find `WesternCalendar` in the examples package.

### Datum and Timestamp
Covella features two primary classes for dates. `Datum` which resembles what we humans usually do, and `Timestamp` which is a very long number and the format prefered by computers. You can enter a date as a string in international format: Highest unit to lowest, seprated by `-` or `:`.

    val january1st1970 = "1970-01-01".dateInCalendar( moreAdvancedCalendar )

You can define your calendar in implicit scope and forgeo the parameter.

We can convert between Datum and Timestamp by calling  
- `Datum::begins`, `Datum::ends` for `Datum=>Option[Timestamp]` and
- `Timestamp::inCalendar(implicit cal: Calendar)` for `Timestamp=>Datum`
respectively.

But in order to do so, our Calendar requires information about what Datum corresponds to `Timestamp(0)`. Let's put that in.

    julianCalendar setTimestampZero Datum.of('year -> 1970)
    
Using January 1st, 1970, which happens to be the unix epoch. 

### Synchronisation
We still lack weeks in our model. Weeks form a system, independent of months and years. We can set them up as their own Calendar.

    val weeks = val weeks = 'week of ('weekday isAliasFor days) withNames
      ("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") withOffset 1

    val planetaryWeek = Calendar(weeks) setTimestampZero Date.of('week ->0, 'weekday->4)
                  
We set `timeStampZero`, to same point in time as our year-and-months calendar: 1970 started on a Thursday (fourth day of the week). The 0 denotes the week, which will keep counting up for all eternity, but we probably don't care about that.

    val myFirstCalendarSystem = moreAdvancedCalendar synchronize planetaryWeek
    
You can synchronise as many simple calendars as you like. Note that order matters: If units in several calendars have the same designation, all information concerning will be interpreted in terms of the first calendar where it occurs. Also entering dates as a String like we did above will only take into account the first calendar. For more advanced formatting you need:

### Date formats

Our calendar is now complete and we are able to create dates. But we also want to output dates as strings or parse strings into dates. This can be done with the DateFormat class. The library provides some simple DateFormats in the package object. You can compose DateFormats in a StringContext.

    val germanFormat = df"$d.$m.$y"
    
This string interpolation provides a DateFormat of 'dd.mm.yyyy' complete with formatter and parsers. You can then use `Calendar::parse` and `Datum::format` with the DateFormat in implicit scope.
