fun is_older (date1 : int * int * int, date2 : int * int * int) =
    let
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2
        orelse (y1 = y2 andalso m1 < m2)
        orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end
    
fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
        (if (#2 (hd dates)) = month then 1 else 0) + number_in_month(tl dates, month)

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else 
        if (#2 (hd dates)) = month
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int * int * int) list, months : int list) = 
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(words : string list, n : int) = 
    if n = 1
    then hd words
    else get_nth(tl words, n - 1)

fun date_to_string(date : int * int * int) = 
    let
        val months = ["January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, nums : int list) = 
    if hd nums >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd nums, tl nums)

fun what_month(day : int) = 
    let
        val months = [31,28,31,30,31,30,31,31,30,31,30,31];
    in
        1 + number_before_reaching_sum(day, months)
    end

fun month_range(day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else
        what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) = 
    if null dates
    then NONE
    else 
        let
            val tmp = oldest(tl dates)
        in
            if isSome tmp andalso is_older(valOf tmp, hd dates) then tmp else SOME (hd dates)
        end


(* challenge problem 12 *)

fun value_exists_in_list(target : int, values : int list) = 
    if null values
    then false
    else hd values = target orelse value_exists_in_list(target, tl values)

fun remove_duplicates(values : int list) = 
    if null values
    then []
    else
        let 
            val tmp = remove_duplicates(tl values)
        in
            if value_exists_in_list(hd values, tmp) then tmp else hd values :: tmp
        end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) = 
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date : int * int * int) =
    let 
        val year = #1 date
        val month = #2 date
        val day = #3 date
        fun is_leap_year(year : int) = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        fun reasonable_year(year : int) = year > 0
        fun reasonable_month(month : int) = month > 0 andalso month <= 12
        fun reasonable_day(year : int, month : int, day : int) =
            if month = 2
            then
                if is_leap_year(year) then day > 0 andalso day <= 29 else day > 0 andalso day <= 28
            else
                if value_exists_in_list(month, [1,3,5,7,8,10,12])
                then day > 0 andalso day <= 31
                else day > 0 andalso day <= 30
    in
        reasonable_year(year) andalso reasonable_month(month) andalso reasonable_day(year, month, day)
    end