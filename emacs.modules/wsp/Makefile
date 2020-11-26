SANDBOX_DIR?=sandbox
WORKSPACE_DIR=$(SANDBOX_DIR)/wsp

.PHONE=test
test:
	rm -rf $(WORKSPACE_DIR)
	./makem.sh --sandbox=$(SANDBOX_DIR) --install-deps interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-create)"
	./makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-restore1)"
	./makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-restore2)"
	./makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-close)"
	./makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-switch)"
	./makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-delete)"

	./makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-new-workspace-with-multiple-projects)"
	./makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-new-workspace-with-multiple-projects-restore)"


clean:
	rm -rf $(SANDBOX_DIR)
