# NOTE: This program is NOT perfect.  In some cases, it will not place the date in the correct field.
# These errors can be easily fixed in excel!

# This program parses LexisNexis output
# Example below:
# python LexisNexisParser.py Transcripts2012-05-21_15-21.TXT tv1.tsv tv
# python LexisNexisParser.py newspapertest.txt print1.tsv print

import sys
import re

def main() :

    # Terminate program if not all command line arguments are given
    if len(sys.argv) < 4 :
        sys.exit('Need 3 arguments - input file name, output file name, and whether the file is for TV or print')
        
    # Open input file
    inputFilePointer = open(sys.argv[1],'r')

    # Read input file
    inputFile = inputFilePointer.read()

    # Create output file
    outputFile = open(sys.argv[2],'w')

    # Split input file into individual documents
    documents = re.split(r'\d+ of \d+ DOCUMENTS', inputFile)
    
    # Parse document according to whether document is for TV or print
    if sys.argv[3].lower() == 'tv' : 

        for i in range(1, len(documents)) :
            
            # Write header to output file            
            if i == 1 :
                outputFile.write('Network|Time|Show|Length|Text\n')

            # Strip leading and trailing whitespace
            temp = documents[i].strip()

            # Split document into rows
            tempSplit = temp.split('\r\n')

            # Extract network, time, show and title information
            network = tempSplit[0].strip()
            time = tempSplit[2].strip()
            show = tempSplit[4].strip()

            # Extract length information and remove length information from text
            j = 6
            while "LENGTH:" not in tempSplit[j] and j+2 <= len(tempSplit)-1 :
                j += 2
            if j >= len(tempSplit) - 2 :
                j = 5
                length = ' '
            else :
                length = tempSplit[j].strip()
                tempSplit.pop(j)

            # Extract rest of the text
            text = ' '.join(tempSplit[j:len(tempSplit)-1])

            # Strip newline characters from text
            text = re.sub(r'\s',' ',text)
            text = text.strip()

            # Create row of data
            document = network + '|' + time + '|' + show + '|' + length + '|' + text + '\n'

            # Write row to output file
            outputFile.write(document)

    elif sys.argv[3].lower() == 'print' :

        for i in range(1, len(documents)) :

            # Write header to output file
            if i == 1 :
                outputFile.write('Paper|Time|Title|Byline|Section|Length|Text\n')

            # Strip leading and trailing whitespace
            temp = documents[i].strip()

            # Split document into rows
            tempSplit = re.split("\r\n",temp)

            # Extract paper, time and title information
            paper = tempSplit[0].strip()
            time = tempSplit[2].strip()
            title = tempSplit[3].strip()
            byline = tempSplit[4].strip()
           
            # Extract section and length information
            if 'LENGTH:' in tempSplit[4] :
                section = ' '
                length = tempSplit[4].strip()
                text = ' '.join(tempSplit[5:len(tempSplit)-1])
            else :
                section = tempSplit[4].strip()
                length = tempSplit[5].strip()
                text = ' '.join(tempSplit[6:len(tempSplit)-1])

            # Strip newline characters from text
            text = re.sub(r'\s',' ',text)
            text = text.strip()

            # Create row of data
            document = paper + '|' + time + '|' + title + '|' + byline + '|' + section + '|' + length + '|' + text + '\n'
            
            # Write row to output file
            outputFile.write(document)

    else :
        print 'Error: program can only handle tv or print'
    
    # Close files
    outputFile.close()
    inputFilePointer.close() 

if __name__ == '__main__' :
    main()