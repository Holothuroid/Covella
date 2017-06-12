# Covella
'Juno Covella' is the name the ancient Roman 'King of Sacrifices' called at the beginning of each month.

This package provides a DSL for arbitrary systems of time keeping.
- Create your own time units and calendar.
- Combine several cycles. (Yes, it can do moons.)
- Create immutable dates.
- Define date formats and parsers in a simple manner.

- Yet to come: Perform complex queries on your calendar.

## How to

### Starting with a tick
The following is a presentation of the Western Calendar, that monstrous thing you probably use every day.
We first need a smallest time unit, called a `Tick`. Customarily, the JVM uses milliseconds, so let's do that too.

    val millis = Tick('millisecond) 

### Using Measures for regular time units
Build upon that, using the following syntax. 

    val seconds = 'second is (millis,1000)
    val minutes = 'minute is (seconds,60)
    val hours = 'hour is (minutes,60)
    val days = 'day is (hours,24)


### Getting irregular with Cycles
Years and months can be created in one go.

    val standardYear = 'year isCycleOf 'month madeFrom days comprising
    ((31, "January"),
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
Now, we can turn it into a calendar. 

    val simpleCalendar = Calendar(standardYear)
    
That will work, but now all years are the same. To amend our first attempt, we first need a `leapYear`. We can construct it like the standard year but with a different Feburary. Then we need a way to define our leap rule or "intercalation", if you feel fancy.
 
     val moreAdvancedEra = Era given divisibleBy(4) have leapYear default standardYear
     
An Era instance works like partial function. You can add a condition with `given` followed by `have` and the `Measurable` you want. You can add any number of such pairs. Once you are done, you may use `default` to set a default. Then put the era into a Calendar.

     val moreAdvancedCalendar = Calendar(moreAdvancedEra)

Note that this is not really the calendar we use. The leap rule is more complicated, there is no year 0, and things are really weird in 1582. If you want to savor the thing, find `WesternCalendar` in the examples package.

### Datum and Timestamp
Covella features two primary classes for dates. `Datum` which resembles what we humans usually do, and `Timestamp` which is a very long number and the format prefered by computers. You can enter a date as a string in international format: Highest unit to lowest, seprated by `-` or `:`.

    val january1st1970 = "1970-01-01".inCalendar( moreAdvancedCalendar )

You can define your calendar in implicit scope and forgeo the parameter.

We can convert between Datum and Timestamp by calling  
- `Datum::begins`, `Datum::ends` and `Timestamp::datum(implicit cal: Calendar)` for `Datum=>Option[Timestamp]` and
- `Timestamp::inCalendar(implicit cal: Calendar)` for `Timestamp=>Datum`
respectively.

But in order to do so, our Calendar requires information about what Datum corresponds to `Timestamp(0)`. Let's put that in.

    julianCalendar setTimestampZero "1970"
    
Using January 1st, 1970, which happens to be the unix epoch. 

### Synchronisation
We still lack weeks in our model. Weeks form a system, independent of months and years. We can set them up as their own Calendar.

    val weeks = 'week isCycleOf 'weekday madeFrom days comprising
            ("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

    val planetaryWeek = Calendar(weeks) setTimestampZero "0-Thursday"
                  
We set `timeStampZero`, to same point in time as our year-and-months calendar: 1970 started on a Thrusday. The 0 denotes the week, which will keep counting up for all eternity, but we probably don't care about that.

    val myFirstCalendarSystem = moreAdvancedCalendar synchronize planetaryWeek
    
You can synchronise as many simple calendars as you like. Note that order matters: If units in several calendars have the same designation, all information concerning will be interpreted in terms of the first calendar where it occurs. Also entering dates as a String like we did above will only take into account the first calendar. For more advanced formatting you need:

### Date formats

Our calendar is now complete and we are able to create dates. But we also want to output dates as strings or parse strings into dates. This can be done with the DateFormat class.

    val internationalFormat = df"${num(year,4)}-{num(month)(2)}-{num(day)(2)}"
    
This string interpolation provides a DateFormat complete with formatter and parsers. The standard package features convenience methods, so you can write: `df"$y-$m-$d"`. You can then use `Calendar::parse` and `Datum::format` with the DateFormat in implicit scope.
