// Test: calling enumerate on a non-list type
use metamatch::metamatch;
fn main() {
    metamatch!(42.enumerate());
}
