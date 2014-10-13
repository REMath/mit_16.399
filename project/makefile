.PHONY : all 
all : help non-relational 

.PHONY : help 
help :
	@echo 'non-relational            : non-relational analyses'
	@echo '  iss                     :  initialization & simple sign'
	@echo '  err                     :  error'
	@echo '  int                     :  intervals'
	@echo '  par                     :  parity'
	@echo '  err-int                 :  reduced product of error & intervals'
	@echo '  iss-int                 :  reduced product of sign & intervals'
	@echo '  par-int                 :  reduced product of parity & intervals'
	@echo '  par-iss                 :  reduced product of parity & signs'
	@echo '  par-iss-int             :  reduced product of parity & signs & intervals'

.PHONY : non-relational 
non-relational : iss err int par err-int iss-int par-int par-iss par-iss-int

.PHONY : iss
iss :
	-cd Non-Relational/01-Initialization-Simple-Sign; echo "" > typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make iss >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-f.txt
#
	-cd Non-Relational/01-Initialization-Simple-Sign; echo "" > typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make iss >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fb.txt
#
	-cd Non-Relational/01-Initialization-Simple-Sign; echo "" > typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make iss >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/01-Initialization-Simple-Sign/typescript-iss-fbr.txt

.PHONY : err
err :
	-cd Non-Relational/02-Errors; echo "" > typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/02-Errors/typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/02-Errors/typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/02-Errors/typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/02-Errors/typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/02-Errors/typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make err >> ../Non-Relational/02-Errors/typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/02-Errors/typescript-err-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/02-Errors/typescript-err-f.txt
#
	-cd Non-Relational/02-Errors; echo "" > typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/02-Errors/typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/02-Errors/typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/02-Errors/typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/02-Errors/typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/02-Errors/typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make err >> ../Non-Relational/02-Errors/typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/02-Errors/typescript-err-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/02-Errors/typescript-err-fb.txt
#
	-cd Non-Relational/02-Errors; echo "" > typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/02-Errors/typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/02-Errors/typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/02-Errors/typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/02-Errors/typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/02-Errors/typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make err >> ../Non-Relational/02-Errors/typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/02-Errors/typescript-err-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/02-Errors/typescript-err-fbr.txt

.PHONY : int
int :
	-cd Non-Relational/03-Intervals; echo "" > typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/03-Intervals/typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/03-Intervals/typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/03-Intervals/typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/03-Intervals/typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/03-Intervals/typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make int >> ../Non-Relational/03-Intervals/typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/03-Intervals/typescript-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/03-Intervals/typescript-int-f.txt
#
	-cd Non-Relational/03-Intervals; echo "" > typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make int >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/03-Intervals/typescript-int-fb.txt
#
	-cd Non-Relational/03-Intervals; echo "" > typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make int >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/03-Intervals/typescript-int-fbr.txt

.PHONY : par
par :
	-cd Non-Relational/04-Parity; echo "" > typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/04-Parity/typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/04-Parity/typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/04-Parity/typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/04-Parity/typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/04-Parity/typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make par >> ../Non-Relational/04-Parity/typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/04-Parity/typescript-par-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/04-Parity/typescript-par-f.txt
#
	-cd Non-Relational/04-Parity; echo "" > typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/04-Parity/typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/04-Parity/typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/04-Parity/typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/04-Parity/typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/04-Parity/typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make par >> ../Non-Relational/04-Parity/typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/04-Parity/typescript-par-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/04-Parity/typescript-par-fb.txt
#
	-cd Non-Relational/04-Parity; echo "" > typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/04-Parity/typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/04-Parity/typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/04-Parity/typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/04-Parity/typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/04-Parity/typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make par >> ../Non-Relational/04-Parity/typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/04-Parity/typescript-par-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/04-Parity/typescript-par-fbr.txt

.PHONY : err-int
err-int :
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make err-int >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-err-int-f.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make err-int >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-err-int-fb.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make err-int >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-err-int-fbr.txt

.PHONY : iss-int
iss-int :
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make iss-int >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-iss-int-f.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make iss-int >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fb.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make iss-int >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-iss-int-fbr.txt

.PHONY : par-int
par-int :
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make par-int >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-int-f.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make par-int >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-int-fb.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make par-int >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-int-fbr.txt

.PHONY : par-iss
par-iss :
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make par-iss >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-iss-f.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make par-iss >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fb.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make par-iss >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-iss-fbr.txt

.PHONY : par-iss-int
par-iss-int :
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fbool >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make fassign >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make par-iss-int >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-f.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbbool >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make par-iss-int >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fb.txt
#
	-cd Non-Relational/05-Prod-Red; echo "" > typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make reset >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make notrace >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make nr >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbrbool >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make fbassign >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make par-iss-int >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make examples >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
	-cd Generic-FW-Abstract-Interpreter; make clean >> ../Non-Relational/05-Prod-Red/typescript-par-iss-int-fbr.txt
#
.PHONY : clean
clean :
	-@cd Non-Relational/01-Initialization-Simple-Sign; /bin/rm -f typescript* || true
	-@cd Non-Relational/02-Errors; /bin/rm -f typescript* || true
	-@cd Non-Relational/03-Intervals; /bin/rm -f typescript* || true
	-@cd Non-Relational/04-Parity; /bin/rm -f typescript* || true
	-@cd Non-Relational/05-Prod-Red; /bin/rm -f typescript* || true

