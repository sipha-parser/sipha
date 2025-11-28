---
name: Performance Issue
about: Report a performance problem or suggest a performance improvement
title: '[PERF] '
labels: performance
assignees: ''
---

## Description
Describe the performance issue or improvement you've observed.

## Performance Metrics
- **What is slow?**: (e.g., parsing time, memory usage, incremental updates)
- **Current performance**: 
  - Time: 
  - Memory: 
  - Throughput: 
- **Expected performance**: 
  - Time: 
  - Memory: 
  - Throughput: 

## Reproduction
Steps to reproduce the performance issue:

1. 
2. 
3. 

## Benchmark / Test Case
If possible, provide a benchmark or test case:

```rust
// Benchmark code or minimal reproduction
```

## Environment
- **Rust version**: 
- **Sipha version**: 
- **OS**: 
- **Hardware**: (CPU, RAM, etc.)
- **Feature flags used**: 
- **Backend used**: 
- **Input size**: (e.g., file size, number of tokens)

## Profiling Results
If you've profiled the code, please share:
- Profiling tool used: 
- Key findings: 
- Hot spots identified: 

## Additional Context
- Is this a regression? (Yes / No / Unknown)
- When did you first notice this? 
- Does it occur with different feature flags?
- Any workarounds you've found?

## Proposed Solution (if applicable)
If you have ideas for improving performance, please describe them here.

## Checklist
- [ ] I have provided performance metrics
- [ ] I have included a reproduction case
- [ ] I have checked if this occurs with different configurations
- [ ] I have searched for similar performance issues

