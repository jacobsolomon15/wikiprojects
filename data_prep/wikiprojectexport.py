#wikiprojectexport.py
#This script uses the Special:Export method to retrieve information about all wiki groups


import urllib2, os, time, re

basepath = "data/"

def main():

    #open the file which lists projects and make a list of them
    f = open("wikiproject_list_2013.csv", "r")
    global projects
    projects = {}

    #open a logging file
    global logfile
    logfile = open("log.txt", "w")

    #loop through the file and retrieve the data from each project
    text = f.read()
    lines = text.split("\r")
    for line in lines:
        
        nl = line.split(",")
        link = nl[1]
        
        #Make sure it is actually a wikiproject
        check = re.search("WikiProject", link)
        if not check:
            continue
            
        
        namespace = link[29:]
        shortname = namespace[22:].rstrip()
        projects[namespace] = shortname


    #loop through the projects and perform the export
    counter = 0
    for project in projects:
        
        #if counter > 0:
        #    break
        #counter += 1
        # Delay by one second
        time.sleep(1)
        
        #retrieve the xml file from wikipedia
        
        try:
            retrieve(project)
        except:
            logfile.write("There was a problem with " + project + "\n")
#Method to call the Special:Export function

def retrieve(project):

    logfile.write("retrieving project: " + project + "\n")
    #Build the url

    url = "http://en.wikipedia.org/w/index.php?title=Special:Export&pages=" + project + "&dir=desc&history=true&listauthors=true&templates=false"

    

    #Use a user agent to bypass wikipedia's restrictions for bots
    opener = urllib2.build_opener()   
    opener.addheaders = [('User-agent', 'MyBrowser')]
    infile = opener.open(url)

    #pull out the text and save it to a file
    text = infile.read()

    #define a shortname for the project
    #if len(project) > 21:
    #    shortname = project[22:]
    #    shortname = re.sub("[//,/*]","-", shortname)
    #    
    #else:
    #    shortname = project
    #    shortname = re.sub(":", "-")
    #    logfile.write("fail: " + project + "\n")
    #    print "fail: " + project
    ##make a folder for the project and write the xml from the request
    
    
    
    shortname = projects[project]
    
    
    try:
        os.mkdir(basepath+shortname)
        outfile = open(basepath+shortname+"/"+shortname+".xml", "w")
    
        outfile.write(text)
        logfile.write("Success for: " + project +"**********\n")
    except:
        logfile.write("problem with: " + project + "\n")
    


main()
