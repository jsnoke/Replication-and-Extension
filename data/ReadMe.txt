This file contains basic information describing the data files used in the runs of the actor-oriented models published in 
Berardo, Ramiro, and John T. Scholz. 2010. "Self-Organizing Policy Networks: Risk, Partner Selection, and Cooperation in Estuaries." American Journal of Political Science 54(3):632-649

The files incorporated in this zipped folder are:
“071807dynamic.out”: a file containing the output produced by SIENA. In order to read it, the user will first need to install SIENA.
 “Gov-nongov.dat”: value of “1” if the node is a governmental organization, and “0” otherwise.
“Prodev-proenv1-7.dat”: is a 7-point scale measuring the respondent’s self-reported policy orientation on a predevelopment (7)/ proenvironmental(1) scale. In the file, a value of 11 represents missing data. There are values with one decimal, which results from calculating the mean value of this variable for an organization when there are more than one respondents from the same organization. 

“Trust.dat”: This file contains the values obtained from the following question: “Thinking about the range  of contacts you had with other stakeholders, how much do you completely trust these stakeholders to fulfill promises and obligations made in the context of current or developing estuary policies?” (0 = “complete distrust” to 10 = “complete trust.”). The first column in the file contains the values collected in 1999; the second column contains the values from the second wave of data collection in 2001. The second column contains “11s” when the information was missing. 

“wave 1 network.dat” This is the matrix containing the networks for all the estuaries in 1999. Each estuary’s network is located along the diagonal. A value of 0 represents the absence of a linkage between the organization in the row and the organization in the column, while a 1 represents the presence of the linkage. Outside the main blocks there are only values of 10, which represent structural zeros (organizations in different estuaries cannot be linked to each other). 
“wave 2 network.dat” This is the matrix containing the networks for all estuaries in 2001. Like before, the blocks along the diagonal are the networks. In addition to 0s and 1s, these blocks also contain 9s, which is the number representing missing data. Again, outside the blocks the values are all 10s, representing structural zeros. 

