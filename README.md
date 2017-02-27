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

    val millis = Tick("milliseconds") countFromZero
    
The method `countFromZero` makes sure that you can have 0 milliseconds in a date.

### Using Cycles for regular time units
We can know build upon that. 

    val second = Cycle from millis(1000) named "seconds" countFromZero
    val minutes = Cycle from seconds(60) named "minutes" countFromZero
    val hours = Cycle from minutes(60) named "hours" countFromZero
    val days = Cycle from hours(24) named "days"

We do not want to count days from zero. The first day is day 1.

### Irregular time units
These are days, like most people understand them, i.e. no leap seconds. Above the day are the months and years, which happen to be irregular. We first create time units for them.

    val months = TimeUnit named "months"
    val years = TimeUnit namd "years"
    
We then have to define all twelve months, but I'll leave February to December for you.

    val january = days(31) as month named "January" aka "short" -> "Jan"
    ...
    
We first take 31 days, then assign the unit, then a name and an alias. You can add as many aliases as you like with a key value pair. (The method `named X` is short for `aka "default" -> X`.)

We can then add our twelve months and designate them as a year.

    val standardYear = january + february + march + april + may + june + july + 
                        august + september + october + november + december as year

### Calendars and Eras
Now we can turn it into a calendar. 

    val simpleCalendar = Calendar(standardYear)
    
That will work, but now all years are the same. First we need a leap year. We can construct it like our standard year but with a different feburary.

    val leapFebruary = day(29) as month named "February" aka "short"->"Feb"
    val leapyear = january + leapFebruary + march + april + may + june + july + 
                        august + september + october + november + december as year
                        
 Then we need a way to define our leap rule or "intercalation", if you feel fancy.
 
     val julianEra = Era given _.divisibleBy(4) have leapYear rest standardYear
     
An Era instance works like partial function. You can add a condition with `given` followed by `have` and the thing you want. You can add any number of such pairs. Once you are done, use `rest` to set a default. It also happens that there is no year 0. 1 BCE is followed by 1 CE. So we exclude 0 from our Era. (That also works on any other `imeUnitInstance with Parenting`.)

    julianEra exclude 0

We now have, what we call the Julian Calendar.

     val julianCalendar= Calendar(julianEra)

If you want to implement the Gregorian Calendar feel free to do so or look in the standards package.

### Datum and Timestamp
Covella features two primary classes for dates. `Datum` which resembles what we humans usually do and `Timestamp` which is a very long number and the format prefered by computers.

To enter a Datum, you can use the following syntax.

    val myDatum = year>1970 & month>1 & day>1
    
You can create key-value pairs of time units and some index, separated by `>` and connect them with the `&` operator.

We can convert between Datum and Timestamp by calling  
`Datum::timestamp(implicit cal: Calendar)` and  
`Timestamp::datum(implicit cal: Calendar)`,  
respectively.

But in order to do so our Calendar requires information about what Datum corresponds to Timestamp(0). Let's put that in.

    julianCalendar setTimestampZero myDatum
    
Using January 1st, 1970 happens to be the unix epoch. If you do not specify, our calendar would assume January 1, 1 CE.
