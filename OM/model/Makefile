
all: partial

.PHONY: all clean

partial_20220221-aa.tar.xz:
	wget https://github.com/iotcwpm/SWO/releases/download/v.2.0.20220221/partial_20220221-aa.tar.xz
 
partial_20220221-ab.tar.xz:
	wget https://github.com/iotcwpm/SWO/releases/download/v.2.0.20220221/partial_20220221-ab.tar.xz

partial.tar.xz: partial_20220221-aa.tar.xz partial_20220221-ab.tar.xz
	cat partial_20220221-*.tar.xz > partial.tar.xz

partial: partial.tar.xz
	tar xvf partial.tar.xz

clean:
	rm partial_20220221-*.tar.xz
	rm partial.tar.xz
