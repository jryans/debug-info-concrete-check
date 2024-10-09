## External libraries

There are several ways we might move to external code, including:

- Direct call to current module's jump table, (CF: call, CT: jump)
  then tail call (jump) to external code      (CF: jump, CT: ext)
- Indirect call to external code              (CF: call, CT: ext)
- Tail call (jump) to external code           (CF: jump, CT: ext)
