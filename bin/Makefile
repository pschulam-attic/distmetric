CONFIG = config.json

all: basedata

basedata: bin/python/prep_data.py
	python $< $(CONFIG)

.PHONY: basedata
