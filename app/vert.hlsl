const static float2 positions[3] = {{0.0, -0.5}, {0.5, 0.5}, {-0.5, 0.5}};

const static float3 colors[3] = {{1.0, 1.0, 0.0}, {0.0, 1.0, 0.0}, {1.0, 0.0, 1.0}};

struct VSOutput {
  float4 pos : SV_POSITION;
  [[vk::location(0)]] float3 col;
};

VSOutput main(const uint i : SV_VertexID) {
  VSOutput output;
  output.pos = float4(positions[i], 0, 1.0);
  output.col = colors[i];
  return output;
}
