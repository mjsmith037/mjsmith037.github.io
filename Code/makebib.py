
#### author parsing function
def parseauth(author):
    lastname, firstname = [xx.strip() for xx in author.split(',')]
    initials = firstname.split(' ').strip(' .')
    cc = 'author'
    if ((lastname == 'Smith' or lastname == 'Michalska-Smith') and firstname == 'Matthew J.'):
        cc = cc + ' pi'
    return('<span class="' + cc + '">' + firstname.split(' ')[0][0] + '.' + firstname.split(' ')[1] + ' ' + lastname + '</span>')

#### read in the bibfile
with open('../Documents/MJSbib.bib', 'r') as bibfile:
    bibtext = bibfile.read()

#### parse the information
## split by entry and store each as a dictionary element
bibarray = bibtext.split('@')
bibdict = {}
## get a list of all the years so numbers can be assigned
allyears = [xx[0] for xx in [re.findall(r'ear\s?=\s?{(\d\d\d\d)}', bibitem) for bibitem in bibarray] if xx]

## the stored element will be an html string
for bibitem in bibarray:
    bibinfo = bibitem.split('\n')
    ## extract the parts of the citation
    bibID = bibinfo[0].split('{')[1].strip(',')
    tmp = [xx.split('=') for xx in bibinfo[1:len(bibinfo)]]
    for info in tmp:
        infotype = info[0].strip().lower()
        if infotype == 'title':
            title = info[1].strip(' {},')
        if infotype == 'author':
            authlist = info[1].strip(' {},').replace('}', '').replace('{', '').replace('\\textbf', '').split(' and ')
        if infotype == 'year':
            year = info[1].strip(' {},')
        if infotype == 'journal':
            journal = info[1].strip(' {},')
#        if infotype == '':
#            prelink =
        if infotype == 'doi':
            jrnlink = info[1].strip(' {},')
    #### create the html bibentries
    bibhtml = ['<div class="row publication">',
               '  <div class="col-xs-1"></div>',
               '  <div class="col-xs-8"><a name="$bibID"></a>',
               '    <span class="pubnumber">' + pubnumber +'</span>',
               '    <span class="pubtitle">$title</span><br>',
               ## the author list needs to be parsed and my name accented
               '    ' + ''.join([parseauth(auth)  for auth in authlist]),
               '    <span class="pubyear">$year</span>',
               '  </div>',
               '  <div class="col-xs-2 col-xs-offset-1 text-right biblinks">',
               '    <a href="$preprintlink">',
               '      <span class="label label-info">Citable preprint</span>
               '    </a><br>',
               '    <a href="/pdfs/$bibID">',
               '      <span class="label label-success">Full text PDF</span>',
               '    </a><br>',
               '    <a href="$journallink" title="$journal">',
               '      <span class="label label-warning">Journal version</span>',
               '    </a>',
               '  </div>',
               '</div>']
    bibdict[pubnumber] = '\n'.join(bibhtml)

#### write to file
with open('../bib.html', 'w') as htmlfile:
    for 
    
