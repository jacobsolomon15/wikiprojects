#!/usr/bin/env python


# parses XML files that were downloaded from wikipedia and extracts data into a mysql database

from xml.dom.minidom import parse
import os, time
import MySQLdb as mysql


def main():
    
    #connect to db
    dbuser = ""
    dbname = ""
    dbpass = ""
    dbhost = ""
    try:
        conn = mysql.connect(dbhost, dbuser, dbpass, dbname)

    except mysql.Error, e:
        print e

    c = conn.cursor()
    
    basepath = "wikiprojects2/"
    
    folders = os.listdir(basepath)
    

    # Loop through each project folder
    # process the main page, followed by the talk page
    # There are a few talk pages that weren't retrieved, I kept getting 403 errors
    # These will be printed out
    count = 1
    for project in folders:
        project_name = "Wikipedia:WikiProject_" + project
        project_path = basepath + project + "/" + project + ".xml"
        try:
            dom = parse(project_path)
        
        except IOError:
            continue
        
        talk = 0
        # The arguments for this are:
        # 1. The parsed xml, 2. the mysql cursor, 3. the project id, 4. the project name, 5. whether it is a talk page
        try:
            process(dom, c, count, project, talk)
        except Exception:
            print "problem with", project
        
        
        # Now the talk page
        talk = 1
        talk_path = basepath + project + "/" + project + "_talk.xml"
        
        try:
            domt = parse(talk_path)
        except:
            print project, "talk file not added"
            continue
        try:
            process(domt, c, count, project, talk)
        except:
            print project, "talk file not added- parsing problem"
        
        print count
        count += 1

        ## For testing, just do 10 projects
        #if count > 10:
        #    break
        
    conn.commit()
        
        
def process(dom, c, project_id, project, talk):
    
    #insert into projects table
    if talk == 0:
    
        q = "insert ignore into projects (id, name) values ('%s', '%s')" % (str(project_id), project)
        c.execute(q)
        #print q
    
    
    
    
    for page in dom.getElementsByTagName('page'):
        title = page.getElementsByTagName('title')[0].firstChild.nodeValue 
        page_id = page.getElementsByTagName('id')[0].firstChild.nodeValue
        
        if not title[0:21] == 'Wikipedia:WikiProject': # this weeds out templates
            if talk == 0:
                continue
            else:
                pass
       
        
        revisions = page.getElementsByTagName('revision')
        
        rev_count = 1
        
        for r in revisions:
            rev_id = r.getElementsByTagName('id')[0].firstChild.nodeValue
            timestamp = format_timestamp(r.getElementsByTagName('timestamp')[0].firstChild.nodeValue)
            #print r.getElementsByTagName('timestamp')[0].firstChild.nodeValue
            #break
            
            try:
                contributor = r.getElementsByTagName('contributor')[0].getElementsByTagName('id')[0].firstChild.nodeValue
            
            except IndexError:
                contributor = r.getElementsByTagName('contributor')[0].getElementsByTagName('ip')[0].firstChild.nodeValue
               
            try:
                text = r.getElementsByTagName('text')[0].firstChild.nodeValue
            except AttributeError:
                text = ""
            q = "insert into revisions (editor, size, timestamp, project_id, page_id, rev_id, rank, talk) values ('%s','%s','%s','%s', '%s', '%s', '%s', '%s')" % (str(contributor), str(len(text)), timestamp, str(project_id), str(page_id),str(rev_id), str(rev_count), str(talk))
            c.execute(q)

            
            rev_count += 1
            
            
            
          
def format_timestamp(timestamp):
    t = time.strptime(timestamp, "%Y-%m-%dT%H:%M:%SZ")
    new_timestamp = time.mktime(t)
    return str(new_timestamp)
        
        
main()
    
    
        
