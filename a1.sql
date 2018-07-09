-- COMP3311 18s1 Assignment 1
-- Written by Jason Do (z5159932), April 2018

-- Q1: ...

create or replace view Q1(unswid, name)
as
	SELECT people.unswid, people.name FROM people 
	JOIN (
		SELECT course_enrolments.student FROM course_enrolments 
		GROUP BY course_enrolments.student 
		HAVING COUNT(course_enrolments.student) > 65
	) AS enrolled ON enrolled.student = people.id 
	ORDER BY people.unswid ASC
;

-- Q2: ...

create or replace view Q2(nstudents, nstaff, nboth)
as
	SELECT 
		(SELECT COUNT(students.id) FROM students) AS nstudents,
		(SELECT COUNT(staff.id) FROM staff) AS nstaff,
		(SELECT COUNT(students.id) FROM students INNER JOIN staff ON students.id = staff.id) AS nboth 
;

-- Q3: ...

create or replace view Q3(name, ncourses)
as
	SELECT people.name, COUNT(*) AS ncourses FROM people
	JOIN course_staff As cs ON cs.staff = people.id
	JOIN staff_roles AS sr ON sr.id = cs.role AND sr.name = 'Course Convenor' 
	GROUP BY people.name
	HAVING COUNT(*) = (
		SELECT MAX(cc) FROM (
			SELECT cs.staff, COUNT(cs.role) AS cc FROM course_staff cs GROUP BY cs.staff   
		) AS highest
	)
;

-- Q4: ...

create or replace view Q4a(id)
as
	SELECT people.unswid FROM people
	JOIN program_enrolments AS pe ON pe.student = people.id 
	JOIN semesters ON semesters.year = 2005 AND semesters.term = 'S2' AND semesters.id = pe.semester
	JOIN programs ON programs.code = '3978' AND programs.id = pe.program
;

create or replace view Q4b(id)
as
	SELECT people.unswid FROM people 
	JOIN program_enrolments AS pe ON pe.student = people.id 
	JOIN semesters ON semesters.year = 2005 AND semesters.term = 'S2' AND semesters.id = pe.semester
	JOIN stream_enrolments As se ON se.partOf = pe.id
	JOIN streams ON streams.code = 'SENGA1' AND streams.id = se.stream
;

create or replace view Q4c(id)
as
	SELECT people.unswid FROM people
	JOIN program_enrolments AS pe ON pe.student = people.id
	JOIN programs ON programs.id = pe.program 
	JOIN semesters ON semesters.year = 2005 AND semesters.term = 'S2' AND semesters.id = pe.semester
	JOIN orgunits ON orgunits.id = programs.offeredby AND orgunits.name LIKE '%Computer Science and Engineering%'
;

-- Q5: ...

create or replace view Q5(name)
as
    SELECT orgunits.name FROM orgunits 
	    JOIN orgunit_groups og ON og.owner = orgunits.id
	    JOIN orgunit_types ot ON ot.id = orgunits.utype AND ot.name = 'Faculty' 
	    JOIN (
		    SELECT * FROM orgunits, orgunit_groups og, orgunit_types ot
		    WHERE ot.name = 'Committee' AND ot.id = orgunits.utype AND og.member = orgunits.id
	    ) AS fcommittee ON fcommittee.member = og.member
	    JOIN (
		    SELECT * FROM orgunits, orgunit_groups og, orgunit_types ot
		    WHERE ot.name = 'School' AND ot.id = orgunits.utype AND og.member = orgunits.id
	    ) AS scommittee ON scommittee.owner = og.owner
	    GROUP BY orgunits.name
	    HAVING COUNT(*) = (
		    SELECT MAX(count) AS name FROM (
			    SELECT orgunits.name, COUNT(orgunits.name) AS count FROM orgunits 
			    JOIN orgunit_groups og ON og.owner = orgunits.id
			    JOIN orgunit_types ot ON ot.id = orgunits.utype AND ot.name = 'Faculty' 
			    JOIN (
				    SELECT * FROM orgunits, orgunit_groups og, orgunit_types ot
				    WHERE ot.name = 'Committee' AND ot.id = orgunits.utype AND og.member = orgunits.id
			    ) AS fcommittee ON fcommittee.member = og.member
			    JOIN (
				    SELECT * FROM orgunits, orgunit_groups og, orgunit_types ot
				    WHERE ot.name = 'School' AND ot.id = orgunits.utype AND og.member = orgunits.id
			    ) AS scommittee ON scommittee.owner = og.owner
			    GROUP BY orgunits.name
		    ) AS nice
	    )	
;

-- Q6: ...

create or replace function Q6(integer) returns text
as
$$
	SELECT people.name FROM people WHERE $1 in (people.id, people.unswid); 
$$ language sql
;

-- Q7: ...

create or replace function Q7(text)
	returns table (course text, year integer, term text, convenor text)
as $$
	SELECT DISTINCT $1, semesters.year, CAST(semesters.term AS text), CAST(people.name AS text) FROM people
	JOIN subjects ON subjects.code = $1
	JOIN courses ON courses.subject = subjects.id 
	JOIN semesters ON semesters.id = courses.semester
	JOIN course_staff As cs ON cs.staff = people.id and cs.course = courses.id 
	JOIN staff_roles AS sr ON sr.id = cs.role AND sr.name = 'Course Convenor' 
	ORDER BY semesters.year ASC;
$$ language sql
;

-- Q8: ...

create or replace function Q8(integer)
	returns setof NewTranscriptRecord
as $$
declare
	rec NewTranscriptRecord;
	UOCtotal integer := 0;
	UOCpassed integer := 0;
	wsum integer := 0;
	wam integer := 0;
	x integer;
begin
        select s.id into x
        from   Students s join People p on (s.id = p.id)
        where  p.unswid = $1;
        if (not found) then
                raise EXCEPTION 'Invalid student %',$1;
        end if;
        for rec in
                select  su.code,
                    	substr(t.year::text,3,2)||lower(t.term),
						pr.code,
                        substr(su.name,1,20),
                        e.mark, 
						e.grade, 
						su.uoc
                from    People p
                        join Students s on (p.id = s.id)
                        join Course_enrolments e on (e.student = s.id)
                        join Courses c on (c.id = e.course)
                        join Subjects su on (c.subject = su.id)
                        join Semesters t on (c.semester = t.id)
						join Program_enrolments pe on (pe.student = s.id  and pe.semester = t.id)  
						join Programs pr on (pr.id = pe.program)
                where   p.unswid = $1
                order   by t.starting, su.code
        loop
                if (rec.grade = 'SY') then
                        UOCpassed := UOCpassed + rec.uoc;
                elsif (rec.mark is not null) then
                        if (rec.grade in ('PT','PC','PS','CR','DN','HD','A','B','C')) then
                                UOCpassed := UOCpassed + rec.uoc;
                        end if;
                        UOCtotal := UOCtotal + rec.uoc;
                        wsum := wsum + (rec.mark * rec.uoc);
                        if (rec.grade not in ('PT','PC','PS','CR','DN','HD','A','B','C')) then
                                rec.uoc := 0;
                        end if;

                end if;
                return next rec;
        end loop;
        if (UOCtotal = 0) then
                rec := (null,null,null,'No WAM available',null,null,null);
        else
                wam := wsum / UOCtotal;
                rec := (null,null,null,'Overall WAM',wam,null,UOCpassed);
        end if;

        return next rec;
end;
$$ language plpgsql
;
-- Q9: ...
CREATE TYPE typeAndDefinition AS (
	type text,
	definition text
);

create or replace function Q9(integer)
	returns setof AcObjRecord
as $$
declare
	result AcObjRecord;
	subjectCodes text;
	aca typeAndDefinition;
	tabName text;
	acaObjType text;
	acaDefinition text;
	x integer;
begin
    SELECT aog.id INTO x
	FROM Acad_object_groups aog 
	WHERE aog.id = $1;
	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Invalid id %', $1;
	END IF;
    
	SELECT aog.gtype INTO acaObjType 
	FROM   Acad_object_groups aog
	WHERE  aog.id = $1;
	IF ( lower(acaObjType) = 'stream' ) THEN	
		tabName := 'Streams';
	ELSIF ( lower(acaObjType) = 'subject' ) THEN
		tabName := 'Subjects';
	ELSIF ( lower(acaObjType) = 'program' ) THEN 
		tabName := 'Programs';
	END IF;

	FOR subjectCodes IN 
		EXECUTE 'SELECT ' || tabName || '.code 
				FROM ' || tabName
	LOOP 
	    SELECT aog.gtype, aog.definition INTO aca
	    FROM Acad_object_groups aog
	    WHERE aog.id = $1 
	    AND gdefBy = 'pattern' 
	    AND subjectCodes SIMILAR TO '%(' || replace(replace(aog.definition, '#', '_'), ',', '|') || ')%';
		
		IF (aca.definition LIKE 'FREE%') THEN
			result := (aca.type, aca.definition);
		ELSIF (aca.definition LIKE 'GEN%') THEN
			result := (aca.type, aca.definition);
		ELSIF (acaDefinition LIKE 'ZGEN%') THEN
			result := (aca.type, aca.definition);
		ELSIF (aca.definition LIKE '%{%' ) THEN
		ELSIF (subjectCodes SIMILAR TO '%(' || replace(replace(aca.definition, '#', '_'), ',', '|') || ')%' ) THEN
			result := (aca.type, subjectCodes);
		RETURN NEXT result;
		END IF;
	END LOOP;
end;
$$ language plpgsql
;
