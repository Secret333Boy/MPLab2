(* 1 *)
fun is_older (date1 : int*int*int, date2 : int*int*int) = 
((#1 date1) < (#1 date2)) orelse 
((#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2)) orelse 
((#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) < (#3 date2));
(* 2 *)
fun number_in_month ([], month : int, acc : int) = acc | 
number_in_month ((x : int*int*int)::xs, month : int, acc : int) = number_in_month (xs, month, acc + (if (#2 x) = month then 1 else 0));
(* 3 *)
fun number_in_months (list_of_dates : (int*int*int) list, [], acc) = acc |
number_in_months (list_of_dates : (int*int*int) list, (x : int)::xs, acc) = number_in_months(list_of_dates, xs, acc + number_in_month(list_of_dates, x, 0));
(* 4 *)
fun dates_in_month ([], month : int) = [] |
dates_in_month ((x : int*int*int)::xs, month : int) = if (#2 x) = month then ([x] @ dates_in_month(xs, month)) else dates_in_month(xs, month);
(* 5 *)
fun dates_in_months (list_of_dates : (int*int*int) list, []) = [] |
dates_in_months (list_of_dates : (int*int*int) list, (x : int)::xs) = dates_in_month(list_of_dates, x) @ dates_in_months(list_of_dates, xs);
(* 6 *)
fun get_nth (list_of_strings : string list, 1) = hd list_of_strings |
get_nth (list_of_strings : string list, n : int) = get_nth(tl list_of_strings, n - 1);
(* 7 *)
val months_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
fun date_to_string (date : int*int*int) = get_nth(months_names, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date);
(* 8 *)
fun number_before_reaching_sum (sum : int, (x : int)::xs) = if x < sum then 1 + number_before_reaching_sum(sum, [x + (hd xs)] @ tl xs) else 0;
(* 9 *)
val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
fun what_month (day_of_year : int) = 1 + number_before_reaching_sum(day_of_year, days_of_months);
(* 10 *)
fun month_range (day1 : int, day2 : int) = if day1 > day2 then [] else [day1] @ month_range(day1 + 1, day2);
(* 11 *)
fun legacy ([]) = NONE |
legacy ((x : int*int*int)::xs) = if xs = [] then SOME(x) else (if is_older(x, hd xs) then legacy([x] @ tl xs) else legacy(xs));
(* val res = legacy ([(1999, 12, 11), (2003, 11, 29), (2001, 5, 14)]); *)

(* TESTS *)
fun assertEqual (a1, a2) = if a1 = a2 then "True" else "False";
print("Test1: is_older:\n");
print("1) " ^ assertEqual(is_older((2001, 11, 5), (2002, 10, 2)), true) ^ "\n");
print("2) " ^ assertEqual(is_older((2002, 10, 2), (2002, 10, 2)), false) ^ "\n");
print("3) " ^ assertEqual(is_older((2003, 10, 2), (2002, 10, 2)), false) ^ "\n");

print("Test2: number_in_month:\n");
print("1) " ^ assertEqual(number_in_month([(2003, 11, 8), (2010, 5, 26)], 5, 0), 1) ^ "\n");
print("2) " ^ assertEqual(number_in_month([(2003, 5, 8), (2010, 5, 26)], 5, 0), 2) ^ "\n");
print("3) " ^ assertEqual(number_in_month([(2003, 1, 8), (2010, 2, 26)], 5, 0), 0) ^ "\n");

print("Test3: number_in_months:\n");
print("1) " ^ assertEqual(number_in_months([(2003, 11, 8), (2010, 5, 26)], [5, 11], 0), 2) ^ "\n");
print("2) " ^ assertEqual(number_in_months([(2003, 5, 8), (2010, 3, 26)], [5], 0), 1) ^ "\n");
print("3) " ^ assertEqual(number_in_months([(2003, 1, 8), (2010, 2, 26)], [3], 0), 0) ^ "\n");

print("Test4: dates_in_month:\n");
print("1) " ^ assertEqual(dates_in_month([(2003, 5, 8), (2010, 5, 26)], 5), [(2003, 5, 8), (2010, 5, 26)]) ^ "\n");
print("2) " ^ assertEqual(dates_in_month([(2003, 5, 8), (2010, 3, 26)], 5), [(2003, 5, 8)]) ^ "\n");
print("3) " ^ assertEqual(dates_in_month([(2003, 1, 8), (2010, 2, 26)], 3), []) ^ "\n");

print("Test5: dates_in_months:\n");
print("1) " ^ assertEqual(dates_in_months([(2003, 5, 8), (2010, 8, 26)], [5, 8]), [(2003, 5, 8), (2010, 8, 26)]) ^ "\n");
print("2) " ^ assertEqual(dates_in_months([(2003, 5, 8), (2010, 3, 26)], [5]), [(2003, 5, 8)]) ^ "\n");
print("3) " ^ assertEqual(dates_in_months([(2003, 1, 8), (2010, 2, 26)], [3]), []) ^ "\n");

print("Test6: get_nth:\n");
print("1) " ^ assertEqual(get_nth(months_names, 1), "January") ^ "\n");
print("2) " ^ assertEqual(get_nth(months_names, 3), "March") ^ "\n");
print("3) " ^ assertEqual(get_nth(months_names, 8), "August") ^ "\n");

print("Test7: date_to_string:\n");
print("1) " ^ assertEqual(date_to_string((2002, 10, 08)), "October 8, 2002") ^ "\n");
print("2) " ^ assertEqual(date_to_string((2003, 12, 11)), "December 11, 2003") ^ "\n");

print("Test8: number_before_reaching_sum:\n");
print("1) " ^ assertEqual(number_before_reaching_sum(10, [1, 1, 2, 1, 2, 2, 1]), 6) ^ "\n");
print("2) " ^ assertEqual(number_before_reaching_sum(5, [1, 1, 2, 1, 2, 2, 1]), 3) ^ "\n");
print("3) " ^ assertEqual(number_before_reaching_sum(2, [3, 1, 2, 1, 2, 2, 1]), 0) ^ "\n");

print("Test9: what_month:\n");
print("1) " ^ assertEqual(what_month(334), 11) ^ "\n");
print("2) " ^ assertEqual(what_month(335), 12) ^ "\n");
print("3) " ^ assertEqual(what_month(32), 2) ^ "\n");

print("Test10: month_range:\n");
print("1) " ^ assertEqual(month_range(8, 19), [8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]) ^ "\n");
print("2) " ^ assertEqual(month_range(1, 2), [1, 2]) ^ "\n");

print("Test11: legacy:\n");
print("1) " ^ assertEqual(legacy([(1999, 12, 11), (2003, 11, 29), (2001, 5, 14)]), SOME((1999, 12, 11))) ^ "\n");
print("2) " ^ assertEqual(legacy([]), NONE) ^ "\n");