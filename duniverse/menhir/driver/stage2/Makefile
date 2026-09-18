# The directory used by dune to mirror this directory.

BUILD_DIR := ../../_build/default/driver/stage2

# [make update] is used under the programmer's manual control, after the
# grammar in [Parser.mly] has been modified.

# It updates the file [ParserMessages.messages] with new auto-generated
# comments for all error states.

.PHONY: update
update:
	@ dune build --force ParserMessages.messages.updated
	@ cp $(BUILD_DIR)/ParserMessages.messages.updated ParserMessages.messages

# [make strip] strips away the auto-generated comments found in the file
# ParserMessages.messages. It is typically used after [make update], which
# creates many such comments.

.PHONY: strip
strip:
	@ sed -e "/^##/d" -i.bak ParserMessages.messages
	@ rm ParserMessages.messages.bak
