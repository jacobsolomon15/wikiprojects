#!/usr/bin/env python

# Creates a wide-format csv of listing monthly edits for each editor-project combo over a project's first 3 years

import MySQLdb as mysql
import pickle

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
    
    # Get list of projects
    
    q = "select id from projects"
    c.execute(q)
    projects = c.fetchall()
    
    # A set of timepoints and a label to be used
    variables = {}
    for i in range (1, 62):
        label = "m" + str(i)
        val = i*30
        variables[label] = val
    
    
    data = {}
    
    id = 1
    
    
    # Loop through projects and get all editors
    for p in projects:
        project_id = p[0]
        editors = get_editors(c, project_id)
        
        # output status report
        print project_id, "of 1133"
        
        # Figure out where each editor made their first edit
        for e in editors:
            q = "select min(new_stamp) from revisions where project_id = '%s' and editor = '%s'" % (str(project_id), str(e))
            c.execute(q)
            
            first_stamp = str(c.fetchone()[0])
            data[id] = {"project_id": project_id, "editor" : e, "first_stamp": first_stamp}
            
            for v in variables:
                q = "select count(id) from revisions where project_id = '%s' and new_stamp <= adddate('%s', interval '%s' day) and editor = '%s'" % (str(project_id), first_stamp, str(variables[v]), str(e))
                c.execute(q)
                
                revisions = str(c.fetchone()[0])
                
                data[id][v] = revisions
            
            
            id += 1
            
    make_csv(data, variables)
    
def make_csv(data, variables):
    f = open("wikiprojects_editors.csv", "w")
    
    header = "id,project_id,editor,first_stamp,"
    for v in variables:
        header = header + v +","
    header = header.rstrip(",") + "\n"
    
    text = header
    
    
    for i in range(len(data)):
        line = ""
        for j in data[i]:
            line = line + str(data[i][j]) + ","
        line = line.rstrip(",") + "\n"
        
        text = text + line
        
    f.write(text)
    f.close()
    
def get_editors(c, project_id):
    q = "select distinct(editor) from revisions where project_id = '%s'" % (str(project_id))
    c.execute(q)

    editors = []
    
    r = c.fetchall()
    
    for res in r:
        editors.append(res[0])
        
    return editors

main()
        
        