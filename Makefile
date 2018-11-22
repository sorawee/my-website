run:
	(racket tag-manager.rkt &); \
	(sass --watch scss:css &); \
	raco pollen start

publish:
	export ENVIRON=production; \
	rm -r ../my-website-build; \
	raco pollen reset; \
  raco pollen render -s .; \
	racket tag-manager.rkt --publish; \
	raco pollen publish . ../my-website-build


# posts-sourcefiles := $(wildcard blog/*.poly.pm)
# posts-sourcelistings := $(patsubst %.poly.pm,%.pollen.html,$(posts-sourcefiles))

# all:
# 	touch index.ptree; \
# 	racket utils/tags-generator.rkt; \
# 	raco pollen render index.ptree; \
# 	raco pollen render styles.css; \
# 	raco pollen render feed.xml.pp; \
# 	raco pollen publish . ../build; \
# 	cp -rf ../build/* ../mygithubpage/
