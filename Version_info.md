# Atlas container version, image info


Overall plan:
ghcr.io/lbnl-science-it/atlas are automated build using an ubuntu container, which works for CentOS, but in aws/ubuntu R won't start
tin6150/atlas                 are manual build on centos machine and tag/push to dockerhub.  R works in aws/ubuntu.

eg
FROM ghcr.io/lbnl-science-it/atlas:base     # r-base official image
FROM tin6150/atlas:base                     # same as above, but build manually and pushed to dockerhub, so it works in aws/ubuntu
FROM ghcr.io/lbnl-science-it/atlas:altbase  # Debian or other OS, then install R as package


## specific images 


(docker hub) tin6150/atlas:integrationU : dev image in use by Yuhan.  Build on CentOS host and pushed to docker hub.  R works in aws/Ubuntu instance.  2022-01-27

(docker hub) tin6150/atlas:main : was being used by Ling, broken as of 2022-0203, missing Apollo.

(docker hub) tin6150/atlas:base : R-base (direct) 4.1.1, with Apoollo.  No atlas R code (only R + libs).  manual build on CentOS host, branch base @ 2919bbf 2022-02-03.
``` {bash}
	#ex4: 
	docker build -t tin6150/atlas:base -f Dockerfile.base  .  | tee LOG.Dockerfile.base.2022.0203
	docker push     tin6150/atlas:base
```


tin6150/atlas:main : Atlas Jan test R code bulid on top of tin6150/atlas:base (which was R-base (direct) 4.1.1, with Apoollo.  manual build on CentOS host, branch tbd @ commit-tbd 2022-02-xx.  ## not pushed, tested locally to work on ex4
tbd: ##(docker hub) tin6150/atlas:v1.2 : Atlas 1.2 R code bulid on top of tin6150/atlas:base (which was R-base (direct) 4.1.1, with Apoollo.  manual build on CentOS host, branch tbd @ commit-tbd 2022-02-xx.

``` {bash}
	#ex4: 
	docker build -t tin6150/atlas:basetest -f Dockerfile.atlas .  | tee LOG.Dockerfile.atlas.basetest2
	docker tag      tin6150/atlas:basetest tin6150/atlas:main     # since Ling was using it and it broke
	docker push     tin6150/atlas:main                            # this may need to become the "production" image
```

