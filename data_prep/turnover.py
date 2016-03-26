#!/usr/bin/env python


# Calculates how much turnover in editors each project experiences

import MySQLdb as mysql
import os

def main():
    ## Connect
    dbuser = ""
    dbname = ""
    dbpass = ""
    dbhost = ""
    try:
        conn = mysql.connect(dbhost, dbuser, dbpass, dbname)

    except mysql.Error, e:
        print e

    c = conn.cursor()
    
    
    # Loop through projects
    
    q = "select id from projects"
    c.execute(q)
    
    results = c.fetchall()
    
    for r in results:
        project_id = r[0]
        
        # Get the list of editors
        
        q = "select distinct(editor) from revisions where project_id = '%s' order by new_stamp" % (str(project_id))
        c.execute(q)
        
        editors = c.fetchall()
        
        i = 1
        for e in editors:
            e_id = str(e[0])
            
            q = "select id from revisions where project_id = '%s' and editor = '%s' order by new_stamp" % (str(project_id), e_id)
            c.execute(q)
            
            e_revs = c.fetchall()
            
            j = 1
            for rev in e_revs:
                rev_id = rev[0]
                
                q = "update revisions set editor_rank = '%s', editor_revision_rank = '%s' where id = '%s'" % (str(i), str(j), str(rev_id))
                c.execute(q)
                j += 1
                
            
                
            
            #q = "update revisions set editor_rank = '%s' where project_id = '%s' and editor = '%s'" % (str(i), str(project_id), str(e_id))
            #c.execute(q)
            
            i += 1
        print project_id
            
    q = "update revisions set turnover = editor_rank / editor_revision_rank"
    c.execute(q)
            
    
        
            
            
    conn.commit()
            
            

main() 

