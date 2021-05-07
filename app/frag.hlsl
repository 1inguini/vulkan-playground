float4 main([[vk::location(0)]] const float3 col) : SV_TARGET {
  return float4(col, 1);
}
