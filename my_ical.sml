(*----------------------------------------------------------------------*)

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
        X_MOZ_GENERATION_value:string} list
end  (* sig ICAL *)

structure My_ICAL: MY_ICAL =
struct

        open String;

        (*
        fun chk_ifCharSame a b = 
                if (a = b) then true
                else false;

        val chk_semicolon = chk_ifCharSame #";";
        val chk_colon = chk_ifCharSame #":";
        *)
        
                
                
                
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
                
        fun is_found_semicolon_colon c = 
                if (is_found_semicolon c = true orelse is_found_colon c = true) then true
                else false;
                
        fun scan_first_part ([],left) = (implode(rev(left)),[])
                | scan_first_part (h::t,left) = 
                        if (is_found_semicolon_colon h = true) then (implode(rev(left)), h::t)
                        else scan_first_part (t, h::left);
                
        fun first_part str =
                let
                        val listOfChar = explode(str)
                in 
                        scan_first_part (listOfChar,[])
                end;
                
                
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
                                      
       fun third_part ([]) =  ("","")
                | third_part (h::t) = 
                scan_third_part (t,[],false);
                
                
       
       fun getValues ([], _) = ("","")
                | getValues (L as (h1,h2,h3)::t, keyword) = 
                        if (compare(h1,keyword) = EQUAL) then (h2,h3)
                        else getValues (t, keyword);
                
              
                                
        fun create_record [] =  {DTSTART_param="", DTSTART_value="", DTEND_param="", DTEND_value="", DTSTAMP_param="", DTSTAMP_value="", SEQUENCE_param="", SEQUENCE_value="", SUMMARY_param="", SUMMARY_value="", LOCATION_param="", LOCATION_value="", UID_param="", UID_value="", CREATED_param="", CREATED_value="", LAST_MODIFIED_param="", LAST_MODIFIED_value="", CLASS_param="", CLASS_value="", X_MOZ_GENERATION_param="", X_MOZ_GENERATION_value="", DESCRIPTION_param="", DESCRIPTION_value=""} 
                |  create_record (L) =
                        let
                                val (dtStartParam, dtStartVal) = getValues(L,"DTSTART");
                                val (dtEndParam, dtEndVal) = getValues(L,"DTEND");
                                val (dtStampParam, dtStampVal) = getValues(L,"DTSTAMP");
                                val (sequenceParam, sequenceVal) = getValues(L,"SEQUENCE");
                                val (summaryParam, summaryVal) = getValues(L,"SUMMARY");
                                val (locationParam, locationVal) = getValues(L,"LOCATION");
                                val (uidParam, uidVal) = getValues(L,"UID");
                                val (createdParam, createdVal) = getValues(L,"CREATED");
                                val (lastModifiedParam,lastModifiedVal) = getValues(L,"LAST-MODIFIED");
                                val (classParam, classVal) = getValues(L,"CLASS");
                                val (xmozParam, xmozVal) = getValues(L,"X-MOZ-GENERATION");
                                val (descParam, descVal) = getValues(L,"DESCRIPTION");
                        in
                                 {DTSTART_param=dtStartParam, DTSTART_value=dtStartVal, DTEND_param=dtEndParam, DTEND_value=dtEndVal, DTSTAMP_param=dtStampParam, DTSTAMP_value=dtStampVal, SEQUENCE_param=sequenceParam, SEQUENCE_value=sequenceVal, SUMMARY_param=summaryParam, SUMMARY_value=summaryVal, LOCATION_param=locationParam, LOCATION_value=locationVal, UID_param=uidParam, UID_value=uidVal, CREATED_param=createdParam, CREATED_value=createdVal, LAST_MODIFIED_param=lastModifiedParam, LAST_MODIFIED_value=lastModifiedVal, CLASS_param=classParam, CLASS_value=classVal, X_MOZ_GENERATION_param=xmozParam, X_MOZ_GENERATION_value=xmozVal, DESCRIPTION_param=descParam, DESCRIPTION_value=descVal}
                        end;
                
        
        
        fun create_records [] = []
                | create_records (h::t) = 
                        (create_record h) :: (create_records t);
                
       
       
       fun process_line ("") = ("","","")
                | process_line (line) = 
                let 
                        val one = first_part(line);
                        val two = second_part(#2 one);
                        val three = third_part(#2 one);
                in
                        if (compare(two ,"") = EQUAL) then
                                (#1 one, #1 three, #2 three)
                        else
                                (#1 one, "", two)
                end;
                
                
       fun process_lines_event [] = []
                | process_lines_event (h::t) = 
                        process_line(h)::process_lines_event(t);
        
        
        
        
        fun process_lines ([], eventTupleList, isInsideEvent) = []
                | process_lines (h::t, eventTupleList, isInsideEvent) = 
                        if (compare(h, "BEGIN:VEVENT") = EQUAL) then 
                                process_lines (t, eventTupleList, true)
                                
                        else if (compare(h, "END:VEVENT") = EQUAL) then 
                                rev(eventTupleList)::process_lines (t, [], false)
                                
                        else if (isInsideEvent = true) then
                              process_lines (t, process_line(h)::eventTupleList, true)
                              
                        else
                                process_lines (t, eventTupleList, false);
                                
                                
        fun process_ical2reclist ([]) = []
                | process_ical2reclist (L) = 
                        let
                                val listOfListOfTuples = process_lines (L, [], false);
                                val records = create_records listOfListOfTuples;
                        in
                                records
                        end;
       
        
                                
        
        (*fun getKey ("",_) = ("","")
                | getKey (name, keyword) = 
                        if (compare(name,keyword) = EQUAL) then
                                (name ^ "-" ^ "param", name ^ "-" ^ "value") 
                        else
                                ("", "");*)
        fun readFile (filename:string) = 
                FileIO.readLines (filename);                       
                                
	fun ical2reclist (filename:string) = 
		let 
		        val inputList = readFile (filename);
		in 
		        process_ical2reclist inputList
		end;
		   

end
