#!/usr/bin/env python

# creates a wide-format csv of project growth

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
    
    
    # decide how many projects there are
    q = "select count(id) from projects"
    c.execute(q)
    
    num_projects = int(c.fetchone()[0])
    
    projects = {} # a dict that will hold the sample data
    for i in range(1, num_projects):
        process(i,c, projects)
        
    output = open('wikiprojects_time_data.pkl', 'wb')
    
    pickle.dump(projects, output)
    
    output.close()
    
    make_csv(projects)
        
        
def process(i,c, projects):
    
    print i
    project = {}
        
    #Loop through this list, get measurements for each time period
    
    #First figure out the time the first post was made to the project
    q = "select min(new_stamp), date(min(new_stamp)) from revisions where project_id = '%s'" % (str(i))
    c.execute(q)
    
    results = c.fetchone()
    first_stamp = results[0]
    date_created = results[1]
    
    if not first_stamp:
        return #returns if there are no revisions for a project with that id
    #get the project name and activity
    q = "select name from projects where id = '%s'" % (str(i))
    c.execute(q)
    
    result = c.fetchone()
    
    if not result:
        return
    project['name'] = result[0]
    project['date_created'] = date_created
    #project['active'] = result[1]
    
    
    
    # A set of timepoints and a label to be used
    variables = {}
    for i in range (1, 62):
        label = "m" + str(i)
        val = i*30
        variables[label] = val
        
    #Now fill the dictionary with kinds of measurements- size of the project in characters at each time point, size of project in revisions, size in number of contributors, size in         
    #First do the revision counts
    for v in variables:
        name = v + "_revs"
        
        q = "select count(id) from revisions where project_id = '%s' and new_stamp <= adddate('%s', interval '%s' day) and talk = 0" % (str(i), first_stamp, str(variables[v])) #Figure out how to add the days to the original date
        c.execute(q)
        
        r = c.fetchone()
        try:
            revs = int(r[0])
        except TypeError:
            revs = 'NA'
        
        
        project[name] = revs
        
    #Now do revision sizes
    for v in variables:
        name = v + "_size"
        
        q = "select sum(size) from revisions where project_id = '%s' and new_stamp <= adddate('%s', interval '%s' day) and talk = 0" % (str(i), first_stamp, str(variables[v]))
        c.execute(q)
        
        r = c.fetchone()
        
        try:
            size = int(r[0])
        except TypeError:
            size = 'NA'
        
        
        project[name] = size
        
    #Contributor totals
    for v in variables:
        name = v + "_editors"
        q = "select count(distinct editor) from revisions where project_id = '%s' and new_stamp <= adddate('%s', interval '%s' day)" % (str(i), first_stamp, str(variables[v]))
        c.execute(q)
        
        r = c.fetchone()
        try:
            editors = int(r[0])
        except TypeError:
            editors = 'NA'
        
        project[name] = editors

    #talk pages        
    for v in variables:
        name = v + "_talk"
        
        q = "select count(id) from revisions where project_id = '%s' and new_stamp <= adddate('%s', interval '%s' day) and talk = 1" % (str(i), first_stamp, str(variables[v])) 
        c.execute(q)
        
        r = c.fetchone()
        try:
            talk = int(r[0])
        except TypeError:
            talk = 'NA'
        
        project[name] = talk
        
    # Turnover
    for v in variables:
        name = v + "_turnover"
        
        q = "select sum(turnover) from revisions where project_id = '%s' and new_stamp <= adddate('%s', interval '%s' day)" % (str(i), first_stamp, str(variables[v]))
        c.execute(q)
        
        r = c.fetchone()
        
        try:
            turnover = float(r[0])
        except:
            turnover = 'NA'
            
        project[name] = turnover
        

    
        
    #Now add to the full dictionary
    projects[i] = project
    
    
def make_csv(projects):
    
    # A set of timepoints and a label to be used
    variables = {}
    for i in range (1, 62):
        label = "m" + str(i)
        val = i*30
        variables[label] = val
        
    
    f = open("wikiprojects_time.csv", "w")
    
    
    v_list = []
    
    for v in variables:
        v_list.append(v + "_revs")
        v_list.append(v + "_size")
        v_list.append(v + "_editors")
        v_list.append(v + "_talk")
        v_list.append(v + "_turnover")
        
    #v_list.append("active")
    v_list.append("name")
    v_list.append("date_created")
    
    header = "id,"    
    for var in v_list:
        header = header + var + ","
    
    header = header.rstrip(",") + "\n"
    
    f.write(header)
    
    for p in projects:
        
        line = "'%s'," % (str(p))
        for var2 in v_list:
            line = line + str(projects[p][var2]) + ","
        
        line = line.rstrip(",") + "\n"
        f.write(line)
        
    f.close()
        
        
    
    
    
    
main()

    
    
    
    
    
    
    
    
    

