SIM_DIASCA_TOP = ../../..


.PHONY: doc clean


IMAGE_BASE_NAME := development

DOCKER_USAGE_OPT := --for-dev


doc:
	@echo "   Run 'make docker-image' to create the '$(IMAGE_BASE_NAME)' image, 'make run-docker-image' to test it, then 'make push-docker-image' to push it."


clean: clean-docker-context


# Use the Docker-related explicit rules, such as 'info-docker-image' or
# 'docker-image' to prepare, build and push to registry such image.


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
