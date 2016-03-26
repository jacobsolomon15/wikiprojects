
# Creates a long-format mysql table of project growth

import MySQLdb as mysql


dbuser = "" 
dbname = ""
dbpass = ""  
dbhost = ""
try:
    conn = mysql.connect(dbhost, dbuser, dbpass, dbname)

except mysql.Error, e:
    print e

c = conn.cursor()

c.execute("select max(days_into / 7) from revisions")
max_weeks = c.fetchone()[0]

c.execute("select id from projects")
res = c.fetchall()


for row in res:
  id = row[0]
  print "Doing project %d" % id
  cum_revisions = 0
  cum_talks = 0
  cum_regulars = 0
  cum_size = 0
  cum_editors = 0
  cum_turnover = 0
  for week in range(max_weeks+1):
    # Revisions, size, and editors for this week
    c.execute("SELECT sum(talk) as talks, sum(1-talk) as regular, sum(size) as size, count(distinct editor) as editors, sum(turnover) as turnover from revisions where project_id = %s and days_into >= %s and days_into <= %s", (id, week*7, week*7+6))
    res = c.fetchone()
    talks = res[0] or 0
    regulars = res[1] or 0
    size = res[2] or 0
    editors = res[3] or 0
    turnover = res[4] or 0
    revisions = talks + regulars

    # Calculate cumulative totals when possible
    cum_talks += talks
    cum_regulars += regulars
    cum_revisions += revisions
    cum_size += size
    cum_turnover += turnover

    # Separately select cumulative editors
    c.execute("SELECT count(distinct editor) as editors from revisions where project_id = %s and days_into <= %s", (id, week*7+6))
    cum_editors = c.fetchone()[0]
    c.execute("INSERT INTO growth (project_id, week, revisions, cum_revisions, talks, cum_talks, regulars, cum_regulars, size, cum_size, editors, cum_editors, turnover, cum_turnover) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", (id, week, revisions, cum_revisions, talks, cum_talks, regulars, cum_regulars, size, cum_size, editors, cum_editors, turnover, cum_turnover))
  
  conn.commit()