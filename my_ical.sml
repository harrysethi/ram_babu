(*-------------------------- Harinder Pal (2014MCS2123) --------------------------------------------*)
use "fileIO.sml";

(*--This is a module implementation of "ical & csv translation"--*)

signature MY_ICAL = 
sig
	val ical2reclist : string -> {CLASS_param:string, CLASS_value:string, CREATED_param:string,
        CREATED_value:string, DESCRIPTION_param:string,
        DESCRIPTION_value:string, DTEND_param:string, DTEND_value:string,
        DTSTAMP_param:string, DTSTAMP_value:string, DTSTART_param:string,
        DTSTART_value:string, LAST_MODIFIED_param:string,
        LAST_MODIFIED_value:string, LOCATION_param:string,
        LOCATION_value:string, SEQUENCE_param:string, SEQUENCE_value:string,
        SUMMARY_param:string, SUMMARY_value:string, UID_param:string,
        UID_value:string, X_MOZ_GENERATION_param:string,
        X_MOZ_GENERATION_value:string, ALARM_ACTION_param:string, 
        ALARM_ACTION_value:string, ALARM_DESCRIPTION_param:string, 
        ALARM_DESCRIPTION_value:string, ALARM_SUMMARY_param:string, 
        ALARM_SUMMARY_value:string, ALARM_TRIGGER_param:string, 
        ALARM_TRIGGER_value:string} list
        
        val reclist2csv : {CLASS_param:string, CLASS_value:string, CREATED_param:string,
        CREATED_value:string, DESCRIPTION_param:string,
        DESCRIPTION_value:string, DTEND_param:string, DTEND_value:string,
        DTSTAMP_param:string, DTSTAMP_value:string, DTSTART_param:string,
        DTSTART_value:string, LAST_MODIFIED_param:string,
        LAST_MODIFIED_value:string, LOCATION_param:string,
        LOCATION_value:string, SEQUENCE_param:string, SEQUENCE_value:string,
        SUMMARY_param:string, SUMMARY_value:string, UID_param:string,
        UID_value:string, X_MOZ_GENERATION_param:string,
        X_MOZ_GENERATION_value:string, ALARM_ACTION_param:string, 
        ALARM_ACTION_value:string, ALARM_DESCRIPTION_param:string, 
        ALARM_DESCRIPTION_value:string, ALARM_SUMMARY_param:string, 
        ALARM_SUMMARY_value:string, ALARM_TRIGGER_param:string, 
        ALARM_TRIGGER_value:string} list * string -> unit
        
        val reclist2ical : {CLASS_param:string, CLASS_value:string, CREATED_param:string,
        CREATED_value:string, DESCRIPTION_param:string,
        DESCRIPTION_value:string, DTEND_param:string, DTEND_value:string,
        DTSTAMP_param:string, DTSTAMP_value:string, DTSTART_param:string,
        DTSTART_value:string, LAST_MODIFIED_param:string,
        LAST_MODIFIED_value:string, LOCATION_param:string,
        LOCATION_value:string, SEQUENCE_param:string, SEQUENCE_value:string,
        SUMMARY_param:string, SUMMARY_value:string, UID_param:string,
        UID_value:string, X_MOZ_GENERATION_param:string,
        X_MOZ_GENERATION_value:string, ALARM_ACTION_param:string, 
        ALARM_ACTION_value:string, ALARM_DESCRIPTION_param:string, 
        ALARM_DESCRIPTION_value:string, ALARM_SUMMARY_param:string, 
        ALARM_SUMMARY_value:string, ALARM_TRIGGER_param:string, 
        ALARM_TRIGGER_value:string} list * string -> unit
        
        val ical2csv : string * string -> unit
        
        val csv2reclist : string -> {CLASS_param:string, CLASS_value:string, CREATED_param:string,
        CREATED_value:string, DESCRIPTION_param:string,
        DESCRIPTION_value:string, DTEND_param:string, DTEND_value:string,
        DTSTAMP_param:string, DTSTAMP_value:string, DTSTART_param:string,
        DTSTART_value:string, LAST_MODIFIED_param:string,
        LAST_MODIFIED_value:string, LOCATION_param:string,
        LOCATION_value:string, SEQUENCE_param:string, SEQUENCE_value:string,
        SUMMARY_param:string, SUMMARY_value:string, UID_param:string,
        UID_value:string, X_MOZ_GENERATION_param:string,
        X_MOZ_GENERATION_value:string, ALARM_ACTION_param:string, 
        ALARM_ACTION_value:string, ALARM_DESCRIPTION_param:string, 
        ALARM_DESCRIPTION_value:string, ALARM_SUMMARY_param:string, 
        ALARM_SUMMARY_value:string, ALARM_TRIGGER_param:string, 
        ALARM_TRIGGER_value:string} list
        
       
end  (* sig ICAL *)

structure My_ICAL: MY_ICAL =
struct

        val empty_record = {DTSTART_param="", DTSTART_value="", DTEND_param="", DTEND_value="", DTSTAMP_param="", DTSTAMP_value="", SEQUENCE_param="", SEQUENCE_value="", SUMMARY_param="", SUMMARY_value="", LOCATION_param="", LOCATION_value="", UID_param="", UID_value="", CREATED_param="", CREATED_value="", LAST_MODIFIED_param="", LAST_MODIFIED_value="", CLASS_param="", CLASS_value="", X_MOZ_GENERATION_param="", X_MOZ_GENERATION_value="", DESCRIPTION_param="", DESCRIPTION_value="", ALARM_ACTION_param="", ALARM_ACTION_value="", ALARM_DESCRIPTION_param="", ALARM_DESCRIPTION_value="", ALARM_SUMMARY_param="", ALARM_SUMMARY_value="", ALARM_TRIGGER_param="", ALARM_TRIGGER_value=""};

        fun readFile (filename:string) = 
                FileIO.readLines (filename);
                
                
        fun chk_ifCharSame (a:char, b:char) = 
                if (a = b) then true
                else false;

        fun get_found_char c = 
                if (chk_ifCharSame (c, #";") = true) then #";"
                else if (chk_ifCharSame (c, #":") = true) then #":"
                else #"0";
                
        fun is_found_semicolon c = chk_ifCharSame (#";", c);
        fun is_found_colon c = chk_ifCharSame (#":", c);
        fun is_found_invertedComma c = chk_ifCharSame (#"\"", c);
        fun is_found_comma c = chk_ifCharSame (#",", c);
                
        fun is_found_semicolon_colon c = 
                if (is_found_semicolon c = true orelse is_found_colon c = true) then true
                else false;
                
                
        fun isCharListEqual_IgnoreCase ([], []) = true
                | isCharListEqual_IgnoreCase ([], h_s2::t_s2) = false
                | isCharListEqual_IgnoreCase (h_s1::t_s1, []) = false                
                | isCharListEqual_IgnoreCase (h_s1::t_s1, h_s2::t_s2) = 
                if (Char.compare(Char.toUpper(h_s1), Char.toUpper(h_s2)) = EQUAL) then
                        isCharListEqual_IgnoreCase (t_s1, t_s2)
                else
                        false;
                
        fun isStrEqual_IgnoreCase_helper (s1:string, s2:string) = 
                let
                        val s1_list = explode(s1);
                        val s2_list = explode(s2);
                in
                        isCharListEqual_IgnoreCase (s1_list, s2_list)
                end;
        
        (*--Compares two strings ignoring case--*)
         fun isStrEqual_IgnoreCase (s1:string, s2:string) = 
                if (String.size(s1) <> String.size(s2)) then
                        false
                else
                        isStrEqual_IgnoreCase_helper(s1,s2);
        
        (*-----------------------------------------------------------------------------
         --------------------------------- csv2reclist --------------------------------
         ----------------------------------------------------------------------------*)
        
        (*- state = 0 => outside of any " .... 1 => inside of one " ... 2=> inside of two " -*)
        fun getCsvLineAsList ([], left, state) = []
                | getCsvLineAsList (h::t, left, state) =  
                        if (state = 0 andalso is_found_invertedComma h = true) then
                                getCsvLineAsList (t, left, 1)
                                
                        else if (state = 1 andalso is_found_invertedComma h = false) then
                                getCsvLineAsList (t, h :: left, 1)
                                
                        else if (state = 1 andalso is_found_invertedComma h = true) then        
                                getCsvLineAsList (t, left, 2)
                                
                        else if (state = 2 andalso is_found_invertedComma h = true) then        
                                getCsvLineAsList (t, #"\"" :: left, 1)
                                
                        else if ((state = 0 orelse state = 2) andalso is_found_invertedComma h = false) then
                                 if(is_found_comma h = true) then
                                        implode(rev(left)) :: getCsvLineAsList (t, [], 0)
                                else 
                                        getCsvLineAsList (t, [], state)
                                
                        else
                                getCsvLineAsList (t, left, state);
                                
                
         
        fun getCsvListOfList ([]) =  []
                | getCsvListOfList (h::t) = 
                        (getCsvLineAsList (explode(h ^ ","), [], 0)) :: getCsvListOfList(t);
                        
                        
        fun getValues_csv_helper ([], _, _) = "" 
                | getValues_csv_helper (_, [], _) = ""
                | getValues_csv_helper (header_h::header_t, row_h::row_t, keyword) = 
                        if (isStrEqual_IgnoreCase(header_h, keyword) = true) then row_h
                        else getValues_csv_helper (header_t, row_t, keyword);
                
        fun getValues_csv ([], _, _) = ("", "")
                | getValues_csv (_, [], _) = ("", "")
                | getValues_csv (header, row, keyword) = 
                        (getValues_csv_helper (header, row, keyword ^ "-param"), getValues_csv_helper (header, row, keyword ^ "-value"));
                              
                                
        fun create_record_csv ([],_) =  empty_record
                |  create_record_csv (_,[]) =  empty_record
                |  create_record_csv (header, row) = 
                        let
                                val (dtStartParam, dtStartVal) = getValues_csv(header, row, "DTSTART");
                                val (dtEndParam, dtEndVal) = getValues_csv(header, row, "DTEND");
                                val (dtStampParam, dtStampVal) = getValues_csv(header, row, "DTSTAMP");
                                val (sequenceParam, sequenceVal) = getValues_csv(header, row, "SEQUENCE");
                                val (summaryParam, summaryVal) = getValues_csv(header, row, "SUMMARY");
                                val (locationParam, locationVal) = getValues_csv(header, row, "LOCATION");
                                val (uidParam, uidVal) = getValues_csv(header, row, "UID");
                                val (createdParam, createdVal) = getValues_csv(header, row, "CREATED");
                                val (lastModifiedParam,lastModifiedVal) = getValues_csv(header, row, "LAST-MODIFIED");
                                val (classParam, classVal) = getValues_csv(header, row, "CLASS");
                                val (xmozParam, xmozVal) = getValues_csv(header, row, "X-MOZ-GENERATION");
                                val (descParam, descVal) = getValues_csv(header, row, "DESCRIPTION");
                                val (alarm_actionParam, alarm_actionVal) = getValues_csv(header, row, "ALARM-ACTION");
                                val (alarm_descParam, alarm_descVal) = getValues_csv(header, row, "ALARM-DESCRIPTION");
                                val (alarm_summaryParam, alarm_summaryVal) = getValues_csv(header, row, "ALARM-SUMMARY");
                                val (alarm_triggerParam, alarm_triggerVal) = getValues_csv(header, row, "ALARM-TRIGGER");
                        in
                                 {DTSTART_param=dtStartParam, DTSTART_value=dtStartVal, DTEND_param=dtEndParam, DTEND_value=dtEndVal, DTSTAMP_param=dtStampParam, DTSTAMP_value=dtStampVal, SEQUENCE_param=sequenceParam, SEQUENCE_value=sequenceVal, SUMMARY_param=summaryParam, SUMMARY_value=summaryVal, LOCATION_param=locationParam, LOCATION_value=locationVal, UID_param=uidParam, UID_value=uidVal, CREATED_param=createdParam, CREATED_value=createdVal, LAST_MODIFIED_param=lastModifiedParam, LAST_MODIFIED_value=lastModifiedVal, CLASS_param=classParam, CLASS_value=classVal, X_MOZ_GENERATION_param=xmozParam, X_MOZ_GENERATION_value=xmozVal, DESCRIPTION_param=descParam, DESCRIPTION_value=descVal, ALARM_ACTION_param=alarm_actionParam, ALARM_ACTION_value=alarm_actionVal, ALARM_DESCRIPTION_param=alarm_descParam, ALARM_DESCRIPTION_value=alarm_descVal, ALARM_SUMMARY_param=alarm_summaryParam, ALARM_SUMMARY_value=alarm_summaryVal, ALARM_TRIGGER_param=alarm_triggerParam, ALARM_TRIGGER_value=alarm_triggerVal}
                        end;
                
        
        
        fun create_records_csv ([],_) = []
                | create_records_csv (_,[]) = []
                | create_records_csv (header, rows_h::rows_t) = 
                        (create_record_csv (header, rows_h)) :: (create_records_csv (header, rows_t));
                                      
        
        fun process_csv2reclist ([]) = []
                | process_csv2reclist (L) = 
                        let
                                val csvListOfList = getCsvListOfList (L);
                                val records = create_records_csv (hd(csvListOfList),tl(csvListOfList));
                        in
                                records
                        end;
       
        (*--Converts the csv specified as filename into a list of event records--*)
	fun csv2reclist (filename:string) = 
		let 
		        val inputList = readFile (filename);
		in 
		        process_csv2reclist inputList
		end;
        
        (*-----------------------------------------------------------------------------
         --------------------------------- ical2reclist -------------------------------
         ----------------------------------------------------------------------------*)

        fun scan_first_part ([],left) = (implode(rev(left)),[])
                | scan_first_part (h::t,left) = 
                        if (is_found_semicolon_colon h = true) then (implode(rev(left)), h::t)
                        else scan_first_part (t, h::left);
          
        (*first_part includes getting the prop_name*)        
        fun first_part str =
                let
                        val listOfChar = explode(str)
                in 
                        scan_first_part (listOfChar,[])
                end;
                
       (*second_part includes getting the prop_value*)              
       fun second_part [] = ""
                | second_part (h::t) =
                if (is_found_colon h = true) then
                        implode(t)
                else
                        "";
 
         
        fun scan_third_part ([], left, isInsideInvertedComma) =  (implode(rev(left)),implode([]))
                | scan_third_part (h::t, left, isInsideInvertedComma) =  
                if (isInsideInvertedComma = true andalso is_found_invertedComma h = false) then
                         scan_third_part (t, h::left, true)
                         
                else if (isInsideInvertedComma = true andalso is_found_invertedComma h = true) then
                         scan_third_part (t, h::left, false)
                         
                else if (isInsideInvertedComma = false andalso is_found_invertedComma h = false) then
                         if (is_found_colon h = false) then
                                scan_third_part (t, h::left, false)
                                
                         else 
                                (implode(rev(left)), implode(t))
                         
                else
                         scan_third_part (t, h::left, true);
       
       (*second_part includes getting the param & prop_value*)                                
       fun third_part ([]) =  ("","")
                | third_part (h::t) = 
                scan_third_part (t, [], false);
                
                
       
       fun getValues ([], _) = ("","")
                | getValues (L as (h1, h2, h3)::t, keyword) = 
                        if (isStrEqual_IgnoreCase(h1, keyword) = true) then (h2,h3)
                        else getValues (t, keyword);
                
              
                                
        fun create_record [] =  empty_record
                |  create_record (L) = 
                        let
                                val (dtStartParam, dtStartVal) = getValues(L, "DTSTART");
                                val (dtEndParam, dtEndVal) = getValues(L, "DTEND");
                                val (dtStampParam, dtStampVal) = getValues(L, "DTSTAMP");
                                val (sequenceParam, sequenceVal) = getValues(L, "SEQUENCE");
                                val (summaryParam, summaryVal) = getValues(L, "SUMMARY");
                                val (locationParam, locationVal) = getValues(L, "LOCATION");
                                val (uidParam, uidVal) = getValues(L, "UID");
                                val (createdParam, createdVal) = getValues(L, "CREATED");
                                val (lastModifiedParam,lastModifiedVal) = getValues(L, "LAST-MODIFIED");
                                val (classParam, classVal) = getValues(L, "CLASS");
                                val (xmozParam, xmozVal) = getValues(L, "X-MOZ-GENERATION");
                                val (descParam, descVal) = getValues(L, "DESCRIPTION");
                                val (alarm_actionParam, alarm_actionVal) = getValues(L, "ALARM-ACTION");
                                val (alarm_descParam, alarm_descVal) = getValues(L, "ALARM-DESCRIPTION");
                                val (alarm_summaryParam, alarm_summaryVal) = getValues(L, "ALARM-SUMMARY");
                                val (alarm_triggerParam, alarm_triggerVal) = getValues(L, "ALARM-TRIGGER");
                        in
                                 {DTSTART_param=dtStartParam, DTSTART_value=dtStartVal, DTEND_param=dtEndParam, DTEND_value=dtEndVal, DTSTAMP_param=dtStampParam, DTSTAMP_value=dtStampVal, SEQUENCE_param=sequenceParam, SEQUENCE_value=sequenceVal, SUMMARY_param=summaryParam, SUMMARY_value=summaryVal, LOCATION_param=locationParam, LOCATION_value=locationVal, UID_param=uidParam, UID_value=uidVal, CREATED_param=createdParam, CREATED_value=createdVal, LAST_MODIFIED_param=lastModifiedParam, LAST_MODIFIED_value=lastModifiedVal, CLASS_param=classParam, CLASS_value=classVal, X_MOZ_GENERATION_param=xmozParam, X_MOZ_GENERATION_value=xmozVal, DESCRIPTION_param=descParam, DESCRIPTION_value=descVal, ALARM_ACTION_param=alarm_actionParam, ALARM_ACTION_value=alarm_actionVal, ALARM_DESCRIPTION_param=alarm_descParam, ALARM_DESCRIPTION_value=alarm_descVal, ALARM_SUMMARY_param=alarm_summaryParam, ALARM_SUMMARY_value=alarm_summaryVal, ALARM_TRIGGER_param=alarm_triggerParam, ALARM_TRIGGER_value=alarm_triggerVal}
                        end;
                
        
        
        fun create_records [] = []
                | create_records (h::t) = 
                        (create_record h) :: (create_records t);
                
       
       
       fun process_line ("", _) = ("", "", "")
                | process_line (line, isInsideAlarm) = 
                let 
                        val one = first_part(line);
                        val two = second_part(#2 one);
                        val three = third_part(#2 one);
                in
                        if (isInsideAlarm = false andalso isStrEqual_IgnoreCase(two, "") = true) then
                                (#1 one, #1 three, #2 three)
                                
                        else if (isInsideAlarm = false andalso isStrEqual_IgnoreCase(two, "") = false) then
                                (#1 one, "", two)
                                
                        else if (isInsideAlarm = true andalso isStrEqual_IgnoreCase(two, "") = true) then
                                ("ALARM-" ^ (#1 one), #1 three, #2 three)
                                
                        else if (isInsideAlarm = true andalso isStrEqual_IgnoreCase(two, "") = false) then
                                ("ALARM-" ^ (#1 one), "", two)
                                
                        else ("", "", "")
                end;
                
        
        fun process_lines ([], eventTupleList, isInsideEvent, isInsideAlarm) = []
                | process_lines (h::t, eventTupleList, isInsideEvent, isInsideAlarm) = 
                        if (isInsideEvent = false andalso isInsideAlarm = false andalso isStrEqual_IgnoreCase(h, "BEGIN:VEVENT") = true) then 
                                process_lines (t, eventTupleList, true, false)
                        
                        else if (isInsideEvent = true andalso isInsideAlarm = false andalso isStrEqual_IgnoreCase(h, "BEGIN:VALARM") = true) then 
                                process_lines (t, eventTupleList, true, true)
                                
                        else if (isInsideEvent = true andalso isInsideAlarm = true andalso isStrEqual_IgnoreCase(h, "END:VALARM") = true) then 
                                process_lines (t, eventTupleList, true, false)
                                
                        else if (isInsideEvent = true andalso isInsideAlarm = false andalso isStrEqual_IgnoreCase(h, "END:VEVENT") = true) then 
                                rev(eventTupleList)::process_lines (t, [], false, false)
                                
                        else if (isInsideEvent = true andalso isInsideAlarm = false) then
                                process_lines (t, process_line(h, false) :: eventTupleList, true, false)
                              
                        else if (isInsideEvent = true andalso isInsideAlarm = true) then
                                process_lines (t, process_line(h, true) :: eventTupleList, true, true)
                              
                        else
                                process_lines (t, eventTupleList, false, false);
                                
                                
        fun process_ical2reclist ([]) = []
                | process_ical2reclist (L) = 
                        let
                                val listOfListOfTuples = process_lines (L, [], false, false);
                                val records = create_records listOfListOfTuples;
                        in
                                records
                        end;

        (*Unfolds the input lines*)                      
        fun unfoldInputList (prev, outL, []) = prev :: outL
                | unfoldInputList (prev, outL, h::t) = 
                        if (isStrEqual_IgnoreCase(prev, "") = true) then
                              unfoldInputList (h, outL, t)
                              
                        else if ((isStrEqual_IgnoreCase(h, "") <> true) andalso (String.sub(h,0) = #"\t" orelse String.sub(h,0) = #" ")) then
                                unfoldInputList (prev ^ substring (h,1,size(h)-1), outL, t)
                                
                        else
                                unfoldInputList (h, prev::outL , t);
        
        (*--Converts the ical specified as filename into a list of event records--*)
	fun ical2reclist (filename:string) = 
		let 
		        val inputList = readFile (filename);
		        val unfoldedInputList = rev(unfoldInputList ("", [], inputList));
		in 
		        process_ical2reclist unfoldedInputList
		end;
        
         (*----------------------------------------------------------------------------
         --------------------------------- reclist2ical -------------------------------
         ----------------------------------------------------------------------------*)
         
         fun getParam ("") = ""
                | getParam (param) = 
                        ";" ^ param;
                        
         fun getPropValue ("") = ""
                | getPropValue (propValue) = 
                        ":" ^ propValue;
                        
                        
         fun getKeywordForAlarm (keyword_in) = 
                if(isStrEqual_IgnoreCase(keyword_in, "ALARM-ACTION") = true) then
                        "ACTION"
                        
                else if(isStrEqual_IgnoreCase(keyword_in, "ALARM-DESCRIPTION") = true) then
                        "DESCRIPTION"
                        
                else if(isStrEqual_IgnoreCase(keyword_in, "ALARM-SUMMARY") = true) then
                        "SUMMARY"
                        
                else if(isStrEqual_IgnoreCase(keyword_in, "ALARM-TRIGGER") = true) then
                        "TRIGGER"
                        
                else 
                        keyword_in;
                        
         fun getLineString_Helper (keyword, param, value, L) = 
                let 
                        val t_param = getParam param;
                        val t_value = getPropValue value;
                in
                        if (isStrEqual_IgnoreCase(t_value, "") = true) then
                                L
                        else if (isStrEqual_IgnoreCase(t_param, "") = true) then
                                (getKeywordForAlarm(keyword) ^ t_value) :: L
                        else 
                                (getKeywordForAlarm(keyword) ^ t_param ^ t_value) :: L
                end;
                
                
         fun getLineString (keyword, param, value, L) = 
                let
                        val lineString = getLineString_Helper (keyword, param, value, L);
                in
                        lineString                                
                end;
                
                
         (* used for folding the strings with >75 length*)
         fun foldStringList ([], isFirstLine) = []
                | foldStringList (h::t, isFirstLine) = 
                        if (isFirstLine = true andalso size(h) > 75) then
                                String.substring(h, 0, 75) :: foldStringList (String.substring(h, 75, String.size(h)-75) :: t, false)
                                
                        else if (isFirstLine = false andalso size(h) > 75) then
                                (" " ^ String.substring(h, 0, 75)) :: foldStringList (String.substring(h, 75, String.size(h)-75) :: t, false)


                        else if (isFirstLine = false) then
                                (" " ^ h) :: foldStringList (t, true)
                                
                        else
                                h :: foldStringList (t, true);
                                
                        
         
         fun rec2Fields ({DTSTART_param=dtStartParam, DTSTART_value=dtStartVal, DTEND_param=dtEndParam, DTEND_value=dtEndVal, DTSTAMP_param=dtStampParam, DTSTAMP_value=dtStampVal, SEQUENCE_param=sequenceParam, SEQUENCE_value=sequenceVal, SUMMARY_param=summaryParam, SUMMARY_value=summaryVal, LOCATION_param=locationParam, LOCATION_value=locationVal, UID_param=uidParam, UID_value=uidVal, CREATED_param=createdParam, CREATED_value=createdVal, LAST_MODIFIED_param=lastModifiedParam, LAST_MODIFIED_value=lastModifiedVal, CLASS_param=classParam, CLASS_value=classVal, X_MOZ_GENERATION_param=xmozParam, X_MOZ_GENERATION_value=xmozVal, DESCRIPTION_param=descParam, DESCRIPTION_value=descVal, ALARM_ACTION_param=alarm_actionParam, ALARM_ACTION_value=alarm_actionVal, ALARM_DESCRIPTION_param=alarm_descParam, ALARM_DESCRIPTION_value=alarm_descVal, ALARM_SUMMARY_param=alarm_summaryParam, ALARM_SUMMARY_value=alarm_summaryVal, ALARM_TRIGGER_param=alarm_triggerParam, ALARM_TRIGGER_value=alarm_triggerVal}, outputList) = 
                let 
                        val L1 = "BEGIN:VEVENT" :: outputList;
                        val L2 = getLineString ("DTSTART", dtStartParam, dtStartVal, L1);
                        val L3 = getLineString ("DTEND", dtEndParam, dtEndVal, L2);
                        val L4 = getLineString ("DTSTAMP", dtStampParam, dtStampVal, L3);
                        val L5 = getLineString ("SEQUENCE", sequenceParam, sequenceVal, L4);
                        val L6 = getLineString ("SUMMARY", summaryParam, summaryVal, L5);
                        val L7 = getLineString ("LOCATION", locationParam, locationVal, L6);
                        val L8 = getLineString ("UID", uidParam, uidVal, L7);
                        val L9 = getLineString ("CREATED", createdParam, createdVal, L8);
                        val L10 = getLineString ("LAST-MODIFIED", lastModifiedParam, lastModifiedVal, L9);
                        val L11 = getLineString ("CLASS", classParam, classVal, L10);
                        val L12 = getLineString ("X-MOZ-GENERATION", xmozParam, xmozVal, L11);
                        val L13 = getLineString ("DESCRIPTION", descParam, descVal, L12);
                        val L14 = "BEGIN:VALARM" :: L13;
                        val L15 = getLineString ("ALARM-ACTION", alarm_actionParam, alarm_actionVal, L14);
                        val L16 = getLineString ("ALARM-DESCRIPTION", alarm_descParam, alarm_descVal, L15);
                        val L17 = getLineString ("ALARM-SUMMARY", alarm_summaryParam, alarm_summaryVal, L16);
                        val L18 = getLineString ("ALARM-TRIGGER", alarm_triggerParam, alarm_triggerVal, L17);
                        val L_final = "END:VALARM" :: L18;
                in
                        "END:VEVENT"::L_final
                end;
         
          fun reclist2ical_helper ([],_) = []
                | reclist2ical_helper (h::[], outputList) = 
                        rev( "END:VCALENDAR" :: (rec2Fields(h, outputList)) )
                | reclist2ical_helper (h::t, outputList) = 
                        ( reclist2ical_helper (t,(rec2Fields (h,outputList))) );
         
         (*--Converts the record list into an ics file specified by filename--*)
         fun reclist2ical ([], _) = ()
                | reclist2ical (L, filename:string) = 
                        let
                                val to_be_written_ics = "BEGIN:VCALENDAR"::"VERSION:2.0"::"PRODID:-//SabreDAV//SabreDAV 1.7.6//EN"::"CALSCALE:GREGORIAN"::(reclist2ical_helper (L,[]));
                                val folded_to_be_written = foldStringList (to_be_written_ics, true);
                        in
                                FileIO.writeLines(filename, folded_to_be_written)
                        end;
        
        (*----------------------------------------------------------------------------
         --------------------------------- reclist2csv -------------------------------
         ----------------------------------------------------------------------------*)
         
         
        (* replaces " by ""  .... hic = handleInvertedComma *) 
        fun hic_helper [] = []
                | hic_helper (h::t) = 
                        if(is_found_invertedComma h) then
                                #"\"" :: #"\"" :: hic_helper (t)
                        else 
                                h :: hic_helper (t);
              
        fun hic (str) = 
                let
                        val charList = hic_helper (String.explode(str));
                in
                        String.implode (charList)
                end;
                                  
        (*String separator*)
        val ss =  "\"" ^ "," ^ "\"";

        fun rec2string ({DTSTART_param=dtStartParam, DTSTART_value=dtStartVal, DTEND_param=dtEndParam, DTEND_value=dtEndVal, DTSTAMP_param=dtStampParam, DTSTAMP_value=dtStampVal, SEQUENCE_param=sequenceParam, SEQUENCE_value=sequenceVal, SUMMARY_param=summaryParam, SUMMARY_value=summaryVal, LOCATION_param=locationParam, LOCATION_value=locationVal, UID_param=uidParam, UID_value=uidVal, CREATED_param=createdParam, CREATED_value=createdVal, LAST_MODIFIED_param=lastModifiedParam, LAST_MODIFIED_value=lastModifiedVal, CLASS_param=classParam, CLASS_value=classVal, X_MOZ_GENERATION_param=xmozParam, X_MOZ_GENERATION_value=xmozVal, DESCRIPTION_param=descParam, DESCRIPTION_value=descVal, ALARM_ACTION_param=alarm_actionParam, ALARM_ACTION_value=alarm_actionVal, ALARM_DESCRIPTION_param=alarm_descParam, ALARM_DESCRIPTION_value=alarm_descVal, ALARM_SUMMARY_param=alarm_summaryParam, ALARM_SUMMARY_value=alarm_summaryVal, ALARM_TRIGGER_param=alarm_triggerParam, ALARM_TRIGGER_value=alarm_triggerVal}) = 
                ("\"" ^ hic(classParam) ^ ss ^ hic(classVal) ^ ss ^ hic(createdParam) ^ ss ^ hic(createdVal) ^ ss ^ hic(descParam) ^ ss ^ hic(descVal) ^ ss ^ hic(dtEndParam) ^ ss ^ hic(dtEndVal) ^ ss ^ hic(dtStampParam) ^ ss ^ hic(dtStampVal) ^ ss ^ hic(dtStartParam) ^ ss ^ hic(dtStartVal) ^ ss ^ hic(lastModifiedParam) ^ ss ^ hic(lastModifiedVal) ^ ss ^ hic(locationParam) ^ ss ^ hic(locationVal) ^ ss ^ hic(sequenceParam) ^ ss ^ hic(sequenceVal) ^ ss ^ hic(summaryParam) ^ ss ^ hic(summaryVal) ^ ss ^ hic(uidParam) ^ ss ^ hic(uidVal) ^ ss ^ hic(xmozParam) ^ ss ^ hic(xmozVal) ^ ss ^ hic(alarm_actionParam) ^ ss ^ hic(alarm_actionVal) ^ ss ^ hic(alarm_descParam) ^ ss ^ hic(alarm_descVal) ^ ss ^ hic(alarm_summaryParam) ^ ss ^ hic(alarm_summaryVal) ^ ss ^ hic(alarm_triggerParam) ^ ss ^ hic(alarm_triggerVal) ^ "\"");
        
        fun reclist2csv_helper ([]) = []
                | reclist2csv_helper (h::t) = 
                        (rec2string h) :: (reclist2csv_helper t);
        
        (*--Converts the record list into a csv file specified by filename--*)
        fun reclist2csv ([],_) = ()
        | reclist2csv (L, filename:string) = 
                let 
                        val listOfString = ("\"CLASS-param\",\"CLASS-value\",\"CREATED-param\",\"CREATED-value\",\"DESCRIPTION-param\",\"DESCRIPTION-value\",\"DTEND-param\",\"DTEND-value\",\"DTSTAMP-param\",\"DTSTAMP-value\",\"DTSTART-param\",\"DTSTART-value\",\"LAST-MODIFIED-param\",\"LAST-MODIFIED-value\",\"LOCATION-param\",\"LOCATION-value\",\"SEQUENCE-param\",\"SEQUENCE-value\",\"SUMMARY-param\",\"SUMMARY-value\",\"UID-param\",\"UID-value\",\"X-MOZ-GENERATION-param\",\"X-MOZ-GENERATION-value\",\"ALARM-ACTION-param\",\"ALARM-ACTION-value\",\"ALARM-DESCRIPTION-param\",\"ALARM-DESCRIPTION-value\",\"ALARM-SUMMARY-param\",\"ALARM-SUMMARY-value\",\"ALARM-TRIGGER-param\",\"ALARM-TRIGGER-value\"") :: (reclist2csv_helper L);
                in
                        FileIO.writeLines(filename, listOfString)
                end;
                
        (*----------------------------------------------------------------------------
         --------------------------------- ical2csv ----------------------------------
         ----------------------------------------------------------------------------*)
        
        (*--Converts the ical specified by inputFile into a csv file specified by outputFile--*)
        fun ical2csv (inputFile:string, outputFile:string) = 
                let
                        val recList = ical2reclist inputFile;
                in
                        reclist2csv (recList, outputFile)
                end;

end
