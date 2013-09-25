LANGUAGE:=en

FILES= $(LANGUAGE)/00-title.md \
	$(LANGUAGE)/01-introduction.md \
	$(LANGUAGE)/02-error-handling.md \
	$(LANGUAGE)/03-pattern-matching.md \
	$(LANGUAGE)/04-return-values.md \
	$(LANGUAGE)/05-recursion.md \
	$(LANGUAGE)/06-types.md \
	$(LANGUAGE)/07-anonymous-functions.md \
	$(LANGUAGE)/08-message-passing.md \
	$(LANGUAGE)/09-spawning-processes.md \
	$(LANGUAGE)/10-module-design.md \

OUTPUT_DIR=$(CURDIR)/output
OUTPUT=$(OUTPUT_DIR)/idiomatic-erlang-$(LANGUAGE)

$(OUTPUT_DIR):
	mkdir -p $(OUTPUT_DIR)

pdf: $(FILES) $(OUTPUT_DIR)
	pandoc --toc -o $(OUTPUT).pdf $(FILES)

epub: $(FILES) $(OUTPUT_DIR)
	pandoc --toc -o $(OUTPUT).epub $(FILES)
