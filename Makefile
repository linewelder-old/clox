TARGET_EXEC := clox

CC := gcc
CFLAGS := -g

SRC_DIR := ./src
SRCS := $(shell ls $(SRC_DIR)/*.c)

BUILD_DIR := ./build
DEP_DIR := $(BUILD_DIR)/.deps
DEP_FLAGS = -MT $@ -MMD -MP -MF $(DEP_DIR)/$*.Td
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET_EXEC): $(BUILD_DIR) $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

$(BUILD_DIR):
	mkdir $(BUILD_DIR)

%.o: %.cpp # Delete the default implicit rule.
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c $(DEP_DIR)/%.d
	$(CC) $(DEP_FLAGS) $(CFLAGS) -c $< -o $@

# During compilation dependencies are written to a temporary file in order to
# avoid generating broken dependency files or incorrect timestamps.
# https://make.mad-scientist.net/papers/advanced-auto-dependency-generation/
# Move them to a non-temporary file only after the compilation has
# succeeded.
	mv -f $(DEP_DIR)/$*.Td $(DEP_DIR)/$*.d
	touch $@

DEP_FILES := $(SRCS:$(SRC_DIR)/%.c=$(DEP_DIR)/%.d)
$(DEP_FILES): | $(DEP_DIR)

$(DEP_DIR):
	mkdir -p $(DEP_DIR)

include $(wildcard $(DEP_FILES))

run: $(BUILD_DIR)/$(TARGET_EXEC)
	$(BUILD_DIR)/$(TARGET_EXEC)

clean:
	rm -r $(BUILD_DIR)
